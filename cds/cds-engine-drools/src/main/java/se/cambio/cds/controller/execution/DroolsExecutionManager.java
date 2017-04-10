package se.cambio.cds.controller.execution;

import org.apache.commons.lang.SerializationUtils;
import org.apache.log4j.Logger;
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
import se.cambio.cds.gdl.converters.drools.GDLDroolsConverter;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.parser.GDLParser;
import se.cambio.cds.model.facade.execution.drools.DroolsRuleEngineService;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.ExecutionLogger;
import se.cambio.cds.util.RuleExecutionWMLogger;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.misc.DataValueGenerator;

import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;
import java.util.*;

public class DroolsExecutionManager {

    private Map<String, KieBase> knowledgeBaseCache = null;
    private static final short MAX_KNOWLEDGE_BASE_CACHE = 10;
    private boolean useCache = true;
    private ExecutionLogger logger = null;
    public static final Long DEFAULT_TIMEOUT = 30000L;
    private ArchetypeManager archetypeManager;
    private GDLParser gdlParser;
    private DroolsRuleEngineService droolsRuleEngineService;

    public DroolsExecutionManager(ArchetypeManager archetypeManager) {
        this.archetypeManager = archetypeManager;
        knowledgeBaseCache = Collections.synchronizedMap(new LinkedHashMap<String, KieBase>());
        gdlParser = new GDLParser();
    }

    public ArchetypeManager getArchetypeManager() {
        return archetypeManager;
    }

    public void executeGuides(
            List<GuideDTO> guideDTOs,
            Calendar date,
            Collection<Object> workingMemoryObjects,
            ExecutionLogger executionLogger) {
        KieBase kb;
        if (useCache) {
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


    private void executeGuides(
            List<String> guideIds,
            KieBase knowledgeBase,
            Calendar date,
            Collection<Object> workingMemoryObjects,
            ExecutionLogger executionLogger) {
        final StatelessKieSession session = knowledgeBase.newStatelessKieSession();

        final RuleExecutionWMLogger ruleExecutionWMLogger = new RuleExecutionWMLogger();
        session.addEventListener(ruleExecutionWMLogger);
        if (date == null) {
            date = Calendar.getInstance();
        }
        final DvDateTime currentDateTime = DataValueGenerator.toDvDateTime(date);
        session.setGlobal("$currentDateTime", currentDateTime);
        logger = executionLogger;
        session.setGlobal("$executionLogger", executionLogger);
        session.setGlobal("$bindingMap", new HashMap<ElementInstance, Map<String, Boolean>>());
        session.setGlobal("$execute", true);
        int initSalience = 0;

        List<String> reverseGuideIds = new ArrayList<>(guideIds);
        Collections.reverse(reverseGuideIds);
        for (String guideId : reverseGuideIds) {
            session.setGlobal(getGuideSalienceId(guideId), initSalience);
            initSalience = initSalience + 1000;
        }
        session.execute(workingMemoryObjects);
        executionLogger.setFiredRules(ruleExecutionWMLogger.getFiredRules());
    }

    public void cancelCurrentExecution() {
        if (logger != null) {
            //TODO Cancel current execution is done through the logger... should change this behaviour
            logger.cancelExecution();
        }
    }


    private KieBase getKnowledgeBase(Collection<GuideDTO> guideDTOs) {
        if (guideDTOs == null || guideDTOs.isEmpty()) {
            return null;
        }
        String guideIdsId = getGuideIdsId(guideDTOs);
        KieBase kb = knowledgeBaseCache.get(guideIdsId);
        if (kb == null) {
            kb = generateKnowledgeBase(guideDTOs);
            knowledgeBaseCache.put(guideIdsId, kb);
            if (knowledgeBaseCache.size() > MAX_KNOWLEDGE_BASE_CACHE) {
                //Remove oldest KB in cache
                String oldestGuideIdsId = knowledgeBaseCache.keySet().iterator().next();
                knowledgeBaseCache.remove(oldestGuideIdsId);
                LoggerFactory.getLogger(DroolsExecutionManager.class).warn("KnowledgeBase cache full. Removing oldest KB: " + guideIdsId);
            }
        }
        return kb;
    }


    public void setUseCache(boolean useCache) {
        Logger.getLogger(DroolsExecutionManager.class).warn("USE-CACHE on cds engine changed to '" + useCache + "'");
        this.useCache = useCache;
    }

    public void clearCache() {
        Logger.getLogger(DroolsExecutionManager.class).info("Clearing drools knowledge base cached.");
        knowledgeBaseCache.clear();
    }

    private String getGuideIdsId(Collection<GuideDTO> guideDTOs) {
        List<String> guideIdsIdList = new ArrayList<>();
        for (GuideDTO guideDTO : guideDTOs) {
            guideIdsIdList.add(guideDTO.getId());
        }
        Collections.sort(guideIdsIdList);
        StringBuilder guideIdsIdSB = new StringBuilder();
        for (String guideId : guideIdsIdList) {
            guideIdsIdSB.append(guideId);
        }
        return guideIdsIdSB.toString();
    }

    private void compileGuide(GuideDTO guideDTO) {
        if (!hasGuideObject(guideDTO)) {
            parseGuide(guideDTO);
        }
        Guide guide = (Guide) SerializationUtils.deserialize(guideDTO.getGuideObject());
        byte[] compiledGuide = getDroolsRuleEngineService().compile(guide);
        guideDTO.setCompiledGuide(compiledGuide);
    }

    public static boolean hasGuideObject(GuideDTO guideDTO) {
        return guideDTO.getGuideObject() != null;
    }


    private KieBase generateKnowledgeBase(Collection<GuideDTO> guideDTOs) {
        final KieServices kieServices = KieServices.Factory.get();
        final KieFileSystem kieFileSystem = kieServices.newKieFileSystem();
        final KieRepository kr = kieServices.getRepository();
        for (GuideDTO guideDTO : guideDTOs) {
            Guide guide = getGuide(guideDTO);
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

    private Resource getResource(Guide guide) {
        if (guide == null) {
            return null;
        }
        String compiledGuide = new GDLDroolsConverter(guide, archetypeManager).convertToDrools();
        try {
            return ResourceFactory.newByteArrayResource(compiledGuide.getBytes("UTF8"));
        } catch (UnsupportedEncodingException exception) {
            throw new RuntimeException(exception);
        }
    }

    private Guide getGuide(GuideDTO guideDTO) {
        if (guideDTO != null) {
            if (!hasGuideObject(guideDTO)) {
                parseGuide(guideDTO);
            }
            return (Guide) SerializationUtils.deserialize(guideDTO.getGuideObject());
        } else {
            return null;
        }
    }

    private void parseGuide(GuideDTO guideDTO) {
        try {
            Guide guide = gdlParser.parse(new ByteArrayInputStream(guideDTO.getSource().getBytes("UTF-8")));
            guideDTO.setGuideObject(SerializationUtils.serialize(guide));
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
    }

    private String getGuideSalienceId(String guideId) {
        return "$" + guideId.replaceAll("[^a-zA-Z0-9]+", "") + "_salience";
    }

    public DroolsRuleEngineService getDroolsRuleEngineService() {
        if (droolsRuleEngineService == null) {
            droolsRuleEngineService = new DroolsRuleEngineService(this);
        }
        return droolsRuleEngineService;
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