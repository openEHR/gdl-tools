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
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.util.ExecutionLogger;
import se.cambio.cds.util.RuleExecutionWMLogger;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.ByteArrayInputStream;
import java.io.ObjectInputStream;
import java.util.*;

public class GuideExecutionManager {

    public LinkedHashMap<String, KnowledgeBase> _knowledgeBaseCache = null;
    private static GuideExecutionManager _instance = null;
    private static final short MAX_KNOWLEDGE_BASE_CACHE = 10;
    private boolean _useCache = true;
    private ExecutionLogger _logger = null;

    private static int TIMEOUT_IN_SECONDS = 30;

    private GuideExecutionManager(){
        _knowledgeBaseCache = new LinkedHashMap<String, KnowledgeBase>();
    }

    public static void executeGuides(
            Collection<GuideDTO> guideDTOs,
            Calendar date,
            Collection<Object> workingMemoryObjects,
            ExecutionLogger executionLogger)
            throws InternalErrorException{
        KnowledgeBase kb = null;
        if (getDelegate()._useCache){
            kb =  getKnowledgeBase(guideDTOs);
        }else{
            kb = generateKnowledgeBase(guideDTOs);
        }
        executeGuides(kb, date, workingMemoryObjects, executionLogger);
    }

    public static void setUseCache(boolean useCache){
        getDelegate()._useCache = useCache;
    }

    public static void executeGuides(
            KnowledgeBase knowledgeBase,
            Calendar date,
            Collection<Object> workingMemoryObjects,
            ExecutionLogger executionLogger)
            throws InternalErrorException{
        try{
            final StatelessKnowledgeSession session = knowledgeBase.newStatelessKnowledgeSession();

            final RuleExecutionWMLogger ruleExecutionWMLogger = new RuleExecutionWMLogger();
            session.addEventListener(ruleExecutionWMLogger);
            if (date==null){
                date = Calendar.getInstance();
            }
            session.setGlobal("$currentDateTime",
                    new DvDateTime(
                            date.get(Calendar.YEAR),
                            date.get(Calendar.MONTH)+1,
                            date.get(Calendar.DAY_OF_MONTH),
                            date.get(Calendar.HOUR_OF_DAY),
                            date.get(Calendar.MINUTE),
                            date.get(Calendar.SECOND),
                            date.getTimeZone()));
            getDelegate()._logger = executionLogger;
            session.setGlobal("$executionLogger", executionLogger);
            session.setGlobal("$execute", true);
            session.execute(workingMemoryObjects);
            executionLogger.setFiredRules(ruleExecutionWMLogger.getFiredRules());
        }catch(Exception e){
            e.printStackTrace();
            throw new InternalErrorException(e);
        }
    }

    public static void cancelCurrentExecution(){
        if (getDelegate()._logger!=null){
            //TODO Cancel current execution is done through the logger... should change this behaviour
            getDelegate()._logger.cancelExecution();
        }
    }


    public static KnowledgeBase getKnowledgeBase(Collection<GuideDTO> guideDTOs)
            throws InternalErrorException{
        if (guideDTOs==null || guideDTOs.isEmpty()){
            return null;
        }
        String guideIdsId = getGuideIdsId(guideDTOs);
        KnowledgeBase kb = getDelegate()._knowledgeBaseCache.get(guideIdsId);
        if (kb==null){
            kb = GuideExecutionManager.generateKnowledgeBase(guideDTOs);
            getDelegate()._knowledgeBaseCache.put(guideIdsId, kb);
            if (getDelegate()._knowledgeBaseCache.size()>MAX_KNOWLEDGE_BASE_CACHE){
                //Remove oldest KB in cache
                String oldestGuideIdsId = getDelegate()._knowledgeBaseCache.keySet().iterator().next();
                getDelegate()._knowledgeBaseCache.remove(oldestGuideIdsId);
                Logger.getLogger(GuideExecutionManager.class).warn("KnowledgeBase cache full. Removing oldest KB: "+guideIdsId);
            }
        }
        return kb;
    }

    public static void clearCacheWithGuideId(String guideId){
        Collection<String> guideIdsIdsToBeRemoved = new ArrayList<String>();
        for (String guideIdsId : getDelegate()._knowledgeBaseCache.keySet()) {
            if (guideIdsId.contains(guideId)){
                guideIdsIdsToBeRemoved.add(guideIdsId);
            }
        }
        for (String guideIdsIdToBeRemoved : guideIdsIdsToBeRemoved) {
            getDelegate()._knowledgeBaseCache.remove(guideIdsIdToBeRemoved);
        }
    }

    private static String getGuideIdsId(Collection<GuideDTO> guideDTOs) {
        List<String> guideIdsIdList = new ArrayList<String>();
        for (GuideDTO guideDTO : guideDTOs) {
            guideIdsIdList.add(guideDTO.getIdGuide());
        }
        Collections.sort(guideIdsIdList);
        StringBuffer guideIdsIdSB = new StringBuffer();
        for (String guideId : guideIdsIdList) {
            guideIdsIdSB.append(guideId);
        }
        return guideIdsIdSB.toString();
    }

    public static KnowledgeBase generateKnowledgeBase(Collection<GuideDTO> guideDTOs) {
        ArrayList<KnowledgePackage> knowledgePackages = new ArrayList<KnowledgePackage>();
        for (GuideDTO guideDTO : guideDTOs) {
            if (guideDTO.getCompiledGuide()==null){
                Logger.getLogger(GuideExecutionManager.class).warn("Guide '"+guideDTO.getIdGuide()+"' is not compiled.");
            };
            KnowledgePackage knowledgePackage =
                    GuideExecutionManager.getKnowledgePackage(guideDTO.getCompiledGuide());
            if (knowledgePackage!=null){
                knowledgePackages.add(knowledgePackage);
            }
        }
        final KnowledgeBase knowledgeBase = KnowledgeBaseFactory.newKnowledgeBase();
        if (!knowledgePackages.isEmpty()){
            knowledgeBase.addKnowledgePackages(knowledgePackages);
        }
        return knowledgeBase;
    }

    public static KnowledgePackage getKnowledgePackage(byte[] guiaCompilada){
        if (guiaCompilada==null){
            return null;
        }
        ByteArrayInputStream bais = new ByteArrayInputStream(guiaCompilada);
        ObjectInputStream objInput = null;
        KnowledgePackage knowledgePackage = null;
        try {
            objInput = new ObjectInputStream(bais);
            knowledgePackage = (KnowledgePackage)objInput.readObject();
        } catch (Exception e) {
            ExceptionHandler.handle(e);
            return null;
        }
        return knowledgePackage;
    }

    public static String getGuideString(Collection<GuideDTO> guides){
        StringBuffer guidesStr = new StringBuffer();
        for (GuideDTO guideDTO : guides) {
            guidesStr.append(guideDTO.getGuideSrc());
        }
        return guidesStr.toString();
    }

    public static GuideExecutionManager getDelegate(){
        if (_instance==null){
            _instance = new GuideExecutionManager();
        }
        return _instance;
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