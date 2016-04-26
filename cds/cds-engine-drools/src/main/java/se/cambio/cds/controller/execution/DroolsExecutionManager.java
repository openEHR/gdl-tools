/**
 * @author iago.corbal
 */
package se.cambio.cds.controller.execution;

import org.apache.log4j.Logger;
import org.drools.KnowledgeBase;
import org.drools.KnowledgeBaseFactory;
import org.drools.definition.KnowledgePackage;
import org.drools.runtime.StatelessKnowledgeSession;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.ExecutionLogger;
import se.cambio.cds.util.RuleExecutionWMLogger;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.misc.CdsConfigurationProvider;
import se.cambio.openehr.util.misc.DataValueGenerator;

import java.io.ByteArrayInputStream;
import java.io.ObjectInputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class DroolsExecutionManager {

    public Map<String, KnowledgeBase> _knowledgeBaseCache = null;
    private static DroolsExecutionManager _instance = null;
    private static final short MAX_KNOWLEDGE_BASE_CACHE = 10;
    private boolean _useCache = true;
    private ExecutionLogger _logger = null;
    private Long _timeOutInMillis = null;

    private DroolsExecutionManager() {
        _knowledgeBaseCache = Collections.synchronizedMap(new LinkedHashMap<String, KnowledgeBase>());
    }

    public static void executeGuides(
            List<GuideDTO> guideDTOs,
            Calendar date,
            Collection<Object> workingMemoryObjects,
            ExecutionLogger executionLogger)
            throws InternalErrorException {
        KnowledgeBase kb;
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
            KnowledgeBase knowledgeBase,
            Calendar date,
            Collection<Object> workingMemoryObjects,
            ExecutionLogger executionLogger)
            throws InternalErrorException {
        try {
            final StatelessKnowledgeSession session = knowledgeBase.newStatelessKnowledgeSession();

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
            int initSalience = 0;

            List<String> reverseGuideIds = new ArrayList<>(guideIds);
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


    private static KnowledgeBase getKnowledgeBase(List<GuideDTO> guideDTOs)
            throws InternalErrorException {
        if (guideDTOs == null || guideDTOs.isEmpty()) {
            return null;
        }
        String guideIdsId = getGuideIdsId(guideDTOs);
        KnowledgeBase kb = getDelegate()._knowledgeBaseCache.get(guideIdsId);
        if (kb == null) {
            kb = DroolsExecutionManager.generateKnowledgeBase(guideDTOs);
            getDelegate()._knowledgeBaseCache.put(guideIdsId, kb);
            if (getDelegate()._knowledgeBaseCache.size() > MAX_KNOWLEDGE_BASE_CACHE) {
                //Remove oldest KB in cache
                String oldestGuideIdsId = getDelegate()._knowledgeBaseCache.keySet().iterator().next();
                getDelegate()._knowledgeBaseCache.remove(oldestGuideIdsId);
                Logger.getLogger(DroolsExecutionManager.class).warn("KnowledgeBase cache full. Removing oldest KB: " + guideIdsId);
            }
        }
        return kb;
    }


    public static void setUseCache(boolean useCache) {
        Logger.getLogger(DroolsExecutionManager.class).warn("USE-CACHE on cds engine changed to '" + useCache + "'");
        getDelegate()._useCache = useCache;
    }

    public static void clearCache() {
        Logger.getLogger(DroolsExecutionManager.class).info("Clearing drools knowledge base cached.");
        getDelegate()._knowledgeBaseCache.clear();
    }

    private static String getGuideIdsId(Collection<GuideDTO> guideDTOs) {
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

    private static KnowledgeBase generateKnowledgeBase(List<GuideDTO> guideDTOs) {
        ArrayList<KnowledgePackage> knowledgePackages = new ArrayList<>();
        for (GuideDTO guideDTO : guideDTOs) {
            if (guideDTO.getCompiledGuide() == null) {
                Logger.getLogger(DroolsExecutionManager.class).warn("Guide '" + guideDTO.getId() + "' is not compiled.");
            }
            KnowledgePackage knowledgePackage =
                    DroolsExecutionManager.getKnowledgePackage(guideDTO.getCompiledGuide());
            if (knowledgePackage != null) {
                knowledgePackages.add(knowledgePackage);
            }
        }
        final KnowledgeBase knowledgeBase = KnowledgeBaseFactory.newKnowledgeBase();
        if (!knowledgePackages.isEmpty()) {
            knowledgeBase.addKnowledgePackages(knowledgePackages);
        }
        return knowledgeBase;
    }

    private static KnowledgePackage getKnowledgePackage(byte[] compiledGuide) {
        if (compiledGuide == null) {
            return null;
        }
        ByteArrayInputStream bais = new ByteArrayInputStream(compiledGuide);
        ObjectInputStream objInput;
        KnowledgePackage knowledgePackage;
        try {
            objInput = new ObjectInputStream(bais);
            knowledgePackage = (KnowledgePackage) objInput.readObject();
        } catch (Exception e) {
            ExceptionHandler.handle(e);
            return null;
        }
        return knowledgePackage;
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
                Logger.getLogger(DroolsExecutionManager.class).info("No CDS execution timeout or errors found loading it. Timeout will be disabled.");
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