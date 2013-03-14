/**
 * @author iago.corbal  
 */
package se.cambio.cds.controller.execution;

import java.io.ByteArrayInputStream;
import java.io.ObjectInputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;

import org.drools.KnowledgeBase;
import org.drools.KnowledgeBaseFactory;
import org.drools.definition.KnowledgePackage;
import org.drools.runtime.StatelessKnowledgeSession;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;

import se.cambio.cds.model.facade.execution.util.RuleExecutionWMLogger;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.util.ExecutionLogger;
import se.cambio.cds.util.exceptions.GuideNotCompiledException;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.handlers.ExceptionHandler;

public class GuideExecutionManager {

    public  KnowledgeBase _knowledgeBase = null;
    private String _lastGuideStr = null;
    private static GuideExecutionManager _instance = null; 

    private GuideExecutionManager(){
	
    }

    public static void executeGuides(
	    Collection<GuideDTO> guideDTOs, 
	    Calendar date, 
	    Collection<Object> workingMemoryObjects,
	    ExecutionLogger executionLogger)
		    throws InternalErrorException{
	executeGuides(getKnowledgeBase(guideDTOs), date, workingMemoryObjects, executionLogger);
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
	    session.setGlobal("$executionLogger", executionLogger);
	    session.execute(workingMemoryObjects);
	    executionLogger.setFiredRules(ruleExecutionWMLogger.getFiredRules());
	}catch(Exception e){
		e.printStackTrace();
	    throw new InternalErrorException(e);
	}
    }


    public static KnowledgeBase getKnowledgeBase(Collection<GuideDTO> guideDTOs) 
	    throws InternalErrorException{
	if (guideDTOs==null || guideDTOs.isEmpty()){
	    return null;
	}
	String guideStr = getGuideString(guideDTOs);
	if (getDelegate()._knowledgeBase==null || !guideStr.equals(getDelegate()._lastGuideStr)){
	    getDelegate()._knowledgeBase = GuideExecutionManager.generateKnowledgeBase(guideDTOs);
	    getDelegate()._lastGuideStr = guideStr;
	}
	return getDelegate()._knowledgeBase;
    }

    public static KnowledgeBase generateKnowledgeBase(Collection<GuideDTO> guideDTOs) {
	ArrayList<KnowledgePackage> knowledgePackages = new ArrayList<KnowledgePackage>();
	for (GuideDTO guideDTO : guideDTOs) {
	    if (guideDTO.getCompiledGuide()==null){
		ExceptionHandler.handle(new GuideNotCompiledException(guideDTO.getIdGuide(), guideDTO.getName()));
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