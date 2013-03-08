package se.cambio.cds.controller.cds;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import se.cambio.cds.controller.guide.ElementInstanceCollection;
import se.cambio.cds.controller.guide.GeneratedArchetypeReference;
import se.cambio.cds.controller.guide.GeneratedElementInstanceCollection;
import se.cambio.cds.controller.guide.GuideManager;
import se.cambio.cds.model.facade.ehr.delegate.EHRFacadeDelegateFactory;
import se.cambio.cds.model.facade.execution.vo.ArchetypeReference;
import se.cambio.cds.model.facade.execution.vo.ElementInstance;
import se.cambio.cds.model.facade.kb.delegate.KBFacadeDelegate;
import se.cambio.cds.model.facade.kb.delegate.KBFacadeDelegateFactory;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.util.Domains;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.exceptions.PatientNotFoundException;

public class CDSManager {

    public static Collection<ElementInstance> getElementInstances(
	    String idPatient, 
	    ElementInstanceCollection elementInstanceCollection, 
	    Collection<GuideDTO> guideDTOs, 
	    GuideManager guideManager) 
		    throws PatientNotFoundException, InternalErrorException{


	//Search for EHR elements
	//Query EHR for elements
	if (idPatient!=null){
	    GeneratedElementInstanceCollection ehrGeneratedElementInstances = new GeneratedElementInstanceCollection();
	    //TODO Should not be the complete collection but only those collections on the guideDTOs passed
	    ElementInstanceCollection completeEIC = guideManager.getCompleteElementInstanceCollection();
	    ehrGeneratedElementInstances.addAll(completeEIC.getAllElementInstancesByDomain(Domains.EHR_ID));
	    ehrGeneratedElementInstances.addAll(completeEIC.getAllElementInstancesByDomain(ElementInstanceCollection.EMPTY_CODE));
	    elementInstanceCollection = new ElementInstanceCollection();
	    elementInstanceCollection.addAll(EHRFacadeDelegateFactory.getDelegate().queryEHRElements(idPatient, ehrGeneratedElementInstances));
	}

	KBFacadeDelegate kbfd = KBFacadeDelegateFactory.getDelegate();

	//Search for CDS templates (not looking into 'ANY' Domain)
	Collection<ElementInstance> cdsGeneratedElementInstances = guideManager.getCompleteElementInstanceCollection().getAllElementInstancesByDomain(Domains.CDS_ID);
	Set<String> idTemplates = new HashSet<String>();
	for (ElementInstance elementInstance : cdsGeneratedElementInstances) {
	    String idTemplate = elementInstance.getArchetypeReference().getIdTemplate();
	    if (idTemplate!=null){
		idTemplates.add(idTemplate);
	    }
	}

	//Query KB for CDS elements
	if (!idTemplates.isEmpty()){
	    elementInstanceCollection.addAll(kbfd.getKBElementsByIdTemplate(idTemplates));
	}

	//Check for missing elements
	checkForMissingElements(elementInstanceCollection, guideManager);
	return elementInstanceCollection.getAllElementInstances();
    }

    public static void checkForMissingElements(
	    ElementInstanceCollection elementInstanceCollection, 
	    GuideManager guideManager){
	//Check for guide elements, if not present, create archetype reference
	for (ArchetypeReference archetypeReference : guideManager.getCompleteElementInstanceCollection().getAllArchetypeReferences()) {
	    GeneratedArchetypeReference generatedArchetypeReference = (GeneratedArchetypeReference)archetypeReference;
	    boolean matches = elementInstanceCollection.matches(generatedArchetypeReference, guideManager);
	    if (!matches){
		elementInstanceCollection.add(archetypeReference, guideManager);
	    }
	}
    }
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  1.1 (the 'License'); you may not use this file except in compliance with
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