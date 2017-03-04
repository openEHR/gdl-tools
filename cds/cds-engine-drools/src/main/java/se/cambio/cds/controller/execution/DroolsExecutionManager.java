package se.cambio.cds.controller.execution;

import org.kie.api.KieBase;
import org.kie.api.KieServices;
import org.kie.api.builder.KieBuilder;
import org.kie.api.builder.KieFileSystem;
import org.kie.api.builder.KieRepository;
import org.kie.api.builder.Message;
import org.kie.api.io.Resource;
import org.kie.api.runtime.KieContainer;
import org.kie.api.runtime.StatelessKieSession;
import org.kie.internal.io.ResourceFactory;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.slf4j.LoggerFactory;
import se.cambio.cds.controller.session.data.Guides;
import se.cambio.cds.gdl.converters.drools.GDLDroolsConverter;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.ExecutionLogger;
import se.cambio.cds.util.RuleExecutionWMLogger;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.misc.CdsConfigurationProvider;
import se.cambio.openehr.util.misc.DataValueGenerator;

import java.io.UnsupportedEncodingException;
import java.util.*;

public class DroolsExecutionManager {

    public Map<String, KieBase> _knowledgeBaseCache = null;
    private static DroolsExecutionManager _instance = null;
    private static final short MAX_KNOWLEDGE_BASE_CACHE = 10;
    private boolean _useCache = true;
    private ExecutionLogger _logger = null;
    private Long _timeOutInMillis = null;

    private DroolsExecutionManager() {
        _knowledgeBaseCache = Collections.synchronizedMap(new LinkedHashMap<String, KieBase>());
    }

    public static void executeGuides(
            List<GuideDTO> guideDTOs,
            Calendar date,
            Collection<Object> workingMemoryObjects,
            ExecutionLogger executionLogger)
            throws InternalErrorException {
        KieBase kb;
        if (getDelegate()._useCache) {
            kb = getKnowledgeBase(guideDTOs);
        } else {
            kb = generateKnowledgeBase(guideDTOs);
        }
        List<String> guideIds = new ArrayList<>();
        for (GuideDTO guideDTO : guideDTOs) {
            guideIds.add(guideDTO.getId());
        }
        executeGuides(guideIds, kb, date, workingMemoryObjects, executionLogger);
    }


    private static void executeGuides(
            List<String> guideIds,
            KieBase knowledgeBase,
            Calendar date,
            Collection<Object> workingMemoryObjects,
            ExecutionLogger executionLogger)
            throws InternalErrorException {
        try {
            final StatelessKieSession session = knowledgeBase.newStatelessKieSession();

            final RuleExecutionWMLogger ruleExecutionWMLogger = new RuleExecutionWMLogger();
            session.addEventListener(ruleExecutionWMLogger);
            if (date == null) {
                date = Calendar.getInstance();
            }
            final DvDateTime currentDateTime = DataValueGenerator.toDvDateTime(date);
            session.setGlobal("$currentDateTime", currentDateTime);
            getDelegate()._logger = executionLogger;
            session.setGlobal("$executionLogger", executionLogger);
            session.setGlobal("$bindingMap", new HashMap<ElementInstance, Map<String, Boolean>>());
            session.setGlobal("$execute", true);
            int initSalience = 0;

            List<String> reverseGuideIds = new ArrayList<String>(guideIds);
            Collections.reverse(reverseGuideIds);
            for (String guideId : reverseGuideIds) {
                session.setGlobal(getGuideSalienceId(guideId), initSalience);
                initSalience = initSalience + 1000;
            }
            session.execute(workingMemoryObjects);
            executionLogger.setFiredRules(ruleExecutionWMLogger.getFiredRules());
        } catch (Exception e) {
            e.printStackTrace();
            throw new InternalErrorException(e);
        }
    }

    public static void cancelCurrentExecution() {
        if (getDelegate()._logger != null) {
            //TODO Cancel current execution is done through the logger... should change this behaviour
            getDelegate()._logger.cancelExecution();
        }
    }


    private static KieBase getKnowledgeBase(Collection<GuideDTO> guideDTOs)
            throws InternalErrorException {
        if (guideDTOs == null || guideDTOs.isEmpty()) {
            return null;
        }
        String guideIdsId = getGuideIdsId(guideDTOs);
        KieBase kb = getDelegate()._knowledgeBaseCache.get(guideIdsId);
        if (kb == null) {
            kb = DroolsExecutionManager.generateKnowledgeBase(guideDTOs);
            getDelegate()._knowledgeBaseCache.put(guideIdsId, kb);
            if (getDelegate()._knowledgeBaseCache.size() > MAX_KNOWLEDGE_BASE_CACHE) {
                //Remove oldest KB in cache
                String oldestGuideIdsId = getDelegate()._knowledgeBaseCache.keySet().iterator().next();
                getDelegate()._knowledgeBaseCache.remove(oldestGuideIdsId);
                LoggerFactory.getLogger(DroolsExecutionManager.class).warn("KnowledgeBase cache full. Removing oldest KB: " + guideIdsId);
            }
        }
        return kb;
    }


    public static void setUseCache(boolean useCache) {
        LoggerFactory.getLogger(DroolsExecutionManager.class).warn("USE-CACHE on cds engine changed to '" + useCache + "'");
        getDelegate()._useCache = useCache;
    }

    public static void clearCache() {
        LoggerFactory.getLogger(DroolsExecutionManager.class).info("Clearing drools knowledge base cached.");
        getDelegate()._knowledgeBaseCache.clear();
    }

    private static String getGuideIdsId(Collection<GuideDTO> guideDTOs) {
        List<String> guideIdsIdList = new ArrayList<String>();
        for (GuideDTO guideDTO : guideDTOs) {
            guideIdsIdList.add(guideDTO.getId());
        }
        Collections.sort(guideIdsIdList);
        StringBuffer guideIdsIdSB = new StringBuffer();
        for (String guideId : guideIdsIdList) {
            guideIdsIdSB.append(guideId);
        }
        return guideIdsIdSB.toString();
    }

    private static KieBase generateKnowledgeBase(Collection<GuideDTO> guideDTOs) throws InternalErrorException {
        final KieServices kieServices = KieServices.Factory.get();
        final KieFileSystem kieFileSystem = kieServices.newKieFileSystem();
        final KieRepository kr = kieServices.getRepository();
        for (GuideDTO guideDTO : guideDTOs) {
            Guide guide = Guides.getInstance().getGuide(guideDTO);
            Resource resource = getResource(guide);
            if (resource != null) {
                kieFileSystem.write("src/main/resources/" + guideDTO.getId() + ".drl", resource);
            }
        }
        final KieBuilder kieBuilder = kieServices.newKieBuilder(kieFileSystem);
        kieBuilder.buildAll();
        if (kieBuilder.getResults().hasMessages(Message.Level.ERROR)) {
            throw new RuntimeException("Build Errors:\n" + kieBuilder.getResults().toString());
        }

        final KieContainer kContainer = kieServices.newKieContainer(kr.getDefaultReleaseId());
        return kContainer.getKieBase();
    }

    private static Resource getResource(Guide guide) throws InternalErrorException {
        if (guide == null) {
            return null;
        }
        String compiledGuide = new GDLDroolsConverter(guide, ArchetypeManager.getInstance()).convertToDrools();
        try {
            return ResourceFactory.newByteArrayResource(compiledGuide.getBytes("UTF8"));
        } catch (UnsupportedEncodingException e) {
            throw new InternalErrorException(e);
        }
    }

    public static DroolsExecutionManager getDelegate() {
        if (_instance == null) {
            _instance = new DroolsExecutionManager();
        }
        return _instance;
    }

    public static Long getExecutionTimeOut() {
        if (getDelegate()._timeOutInMillis == null) {
            try {
                getDelegate()._timeOutInMillis = CdsConfigurationProvider.getCdsConfiguration().getCdsExecutionTimeOut();
            } catch (Exception e) {
                LoggerFactory.getLogger(DroolsExecutionManager.class).info("No CDS execution timeout or errors found loading it. Timeout will be disabled.");
            }
            if (getDelegate()._timeOutInMillis == null) {
                getDelegate()._timeOutInMillis = Long.MAX_VALUE;
            }
        }
        return getDelegate()._timeOutInMillis;
    }

    public static String getGuideSalienceId(String guideId) {
        return "$" + guideId.replaceAll("[^a-zA-Z0-9]+", "") + "_salience";
    }
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 2.0/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  2.0 (the 'License'); you may not use this file except in compliance with
 *  the License. You may obtain a copy of the License at
 *  http://www.mozilla.org/MPL/
 *
 *  Software distributed under the License is distributed on an 'AS IS' basis,
 *  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 *  for the specific language governing rights and limitations under the
 *  License.
 *
 *
 *  The Initial Developers of the Original Code are Iago Corbal and Rong Chen.
 *  Portions created by the Initial Developer are Copyright (C) 2012-2013
 *  the Initial Developer. All Rights Reserved.
 *
 *  Contributor(s):
 *
 * Software distributed under the License is distributed on an 'AS IS' basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 *  ***** END LICENSE BLOCK *****
 */