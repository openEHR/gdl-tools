package se.cambio.cds.model.facade.execution.plain;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import se.cambio.cds.controller.execution.GuideExecutionManager;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.model.facade.execution.delegate.RuleExecutionFacadeDelegate;
import se.cambio.cds.model.facade.execution.vo.ArchetypeReference;
import se.cambio.cds.model.facade.execution.vo.ElementInstance;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.facade.execution.vo.RuleReference;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.util.Domains;
import se.cambio.cds.util.ExecutionLogger;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.exceptions.PatientNotFoundException;

public class PlainRuleExecutionFacadeDelegate implements RuleExecutionFacadeDelegate{

    public RuleExecutionResult execute(
	    Collection<GuideDTO> guides, 
	    Collection<ElementInstance> elementInstances,
	    Calendar date)
		    throws InternalErrorException, PatientNotFoundException{
	HashSet<Object> workingMemoryObjects = new HashSet<Object>();
	for (ElementInstance elementInstance : elementInstances) {
	    workingMemoryObjects.add(elementInstance);
	    //If repeated, wont duplicate (it's a Set)
	    workingMemoryObjects.add(elementInstance.getArchetypeReference());
	}
	
	ExecutionLogger executionLogger = new ExecutionLogger();
	if (!guides.isEmpty()){
	    GuideExecutionManager.executeGuides(
		    guides, date, workingMemoryObjects, executionLogger);
	}
	elementInstances = new ArrayList<ElementInstance>();
	Set<ArchetypeReference> modifiedArhetypeReferences = new HashSet<ArchetypeReference>();
	//Search for modified elements
	for (Object object : workingMemoryObjects) {
	    if (object instanceof ElementInstance){
		ElementInstance elementInstance = (ElementInstance)object;
		ArchetypeReference ar = elementInstance.getArchetypeReference();
		if (!Domains.EHR_ID.equals(ar.getIdDomain())){
		    elementInstances.add(elementInstance);
		    modifiedArhetypeReferences.add(elementInstance.getArchetypeReference());
		}
	    }
	}
	/*
	//Search for elements in modified references
	for (Object object : workingMemoryObjects) {
	    if (object instanceof ElementInstance){
		ElementInstance elementInstance = (ElementInstance)object;
		ArchetypeReference ar = elementInstance.getArchetypeReference();
		if (ar! instanceof CDSArchetypeReference ||
			&& modifiedArhetypeReferences.contains(elementInstance.getArchetypeReference())){
		    elementInstances.add(elementInstance);
		}
	    }
	}*/
	List<RuleReference> ruleReferences = 
		GuideUtil.getRuleReferences(executionLogger.getFiredRules());
	return new RuleExecutionResult(elementInstances, executionLogger.getLog(), ruleReferences);
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