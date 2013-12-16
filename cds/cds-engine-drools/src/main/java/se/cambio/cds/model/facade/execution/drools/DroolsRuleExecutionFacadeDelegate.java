package se.cambio.cds.model.facade.execution.drools;

import org.apache.log4j.Logger;
import se.cambio.cds.controller.execution.DroolsExecutionManager;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.util.ExecutionLogger;
import se.cambio.cds.model.facade.execution.delegate.RuleExecutionFacadeDelegate;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.facade.execution.vo.RuleReference;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.Domains;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.PatientNotFoundException;

import java.util.*;

public class DroolsRuleExecutionFacadeDelegate implements RuleExecutionFacadeDelegate{

    public RuleExecutionResult execute(
            String ehrId,
            Collection<GuideDTO> guides,
            Collection<ElementInstance> elementInstances,
            Calendar date)
            throws InternalErrorException, PatientNotFoundException{
        final HashSet<Object> workingMemoryObjects = new HashSet<Object>();
        for (ElementInstance elementInstance : elementInstances) {
            workingMemoryObjects.add(elementInstance);
            //If repeated, wont duplicate (it's a Set)
            workingMemoryObjects.add(elementInstance.getArchetypeReference());
        }

        final ExecutionLogger executionLogger = new ExecutionLogger();
        if (!guides.isEmpty()){
            Logger.getLogger(DroolsRuleExecutionFacadeDelegate.class).debug("Executing "+guides.size()+" guides using "+workingMemoryObjects.size()+" objects.");
            DroolsExecutionManager.executeGuides(
                    guides, date, workingMemoryObjects, executionLogger);
        }
        final Set<ArchetypeReference> modifiedArhetypeReferences = new HashSet<ArchetypeReference>();
        //Search for modified elements
        for (Object object : workingMemoryObjects) {
            if (object instanceof ElementInstance){
                ElementInstance elementInstance = (ElementInstance)object;
                ArchetypeReference ar = elementInstance.getArchetypeReference();
                if (Domains.CDS_ID.equals(ar.getIdDomain())){
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
        final List<RuleReference> ruleReferences =
                GuideUtil.getRuleReferences(executionLogger.getFiredRules());
        return new RuleExecutionResult(ehrId, modifiedArhetypeReferences, executionLogger.getLog(), ruleReferences);
    }

    @Override
    public void cancelExecution() {
        DroolsExecutionManager.cancelCurrentExecution();
    }

    @Override
    public void clearCache() {
        DroolsExecutionManager.clearCache();
    }

    @Override
    public void setUseCache(boolean useCache){
        DroolsExecutionManager.setUseCache(useCache);
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