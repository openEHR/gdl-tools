package se.cambio.cds.model.facade.cds.plain;

import java.util.Calendar;
import java.util.Collection;

import se.cambio.cds.controller.cds.CDSManager;
import se.cambio.cds.controller.guide.ElementInstanceCollection;
import se.cambio.cds.controller.guide.GuideManager;
import se.cambio.cds.model.facade.cds.delegate.CDSFacadeDelegate;
import se.cambio.cds.model.facade.execution.delegate.RuleExecutionFacadeDelegate;
import se.cambio.cds.model.facade.execution.delegate.RuleExecutionFacadeDelegateFactory;
import se.cambio.cds.model.facade.execution.vo.ElementInstance;
import se.cambio.cds.model.facade.execution.vo.ExecutionMode;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.guide.dao.GenericGuideFactory;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.util.exceptions.GuideNotFoundException;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.exceptions.PatientNotFoundException;

public class PlainCDSFacadeDelegate implements CDSFacadeDelegate{

    private GuideManager _guideManager = null;
    
    public PlainCDSFacadeDelegate(){
    }

    public RuleExecutionResult monitor(
	    String idPatient, 
	    ElementInstanceCollection elementInstanceCollection) 
	    throws InternalErrorException,
	    PatientNotFoundException {
	return monitor(idPatient, elementInstanceCollection, ExecutionMode.NORMAL);
    }

    public RuleExecutionResult monitor(
	    String idPatient,
	    ElementInstanceCollection elementInstancesCollection,
	    ExecutionMode executionMode) throws InternalErrorException,
	    PatientNotFoundException {
	return monitor(idPatient, elementInstancesCollection, null, executionMode);
    }

    public RuleExecutionResult monitor(
	    String idPatient,
	    ElementInstanceCollection elementInstancesCollection, 
	    Calendar date,
	    ExecutionMode executionMode) throws InternalErrorException,
	    PatientNotFoundException {
	Collection<GuideDTO> guides = getGuideManager().getGuides(executionMode, elementInstancesCollection);
	return monitor(idPatient, guides, elementInstancesCollection, date);
    }
    
    public RuleExecutionResult monitor(
	    String idPatient,
	    Collection<GuideDTO> guides,
	    ElementInstanceCollection elementInstancesCollection, 
	    Calendar date) throws InternalErrorException,
	    PatientNotFoundException {
	Collection<ElementInstance> elementInstances = CDSManager.getElementInstances(idPatient, elementInstancesCollection, guides, getGuideManager());
	RuleExecutionFacadeDelegate refd = RuleExecutionFacadeDelegateFactory.getDelegate();
	return refd.execute(guides, elementInstances, date);
    }
    
    public Collection<ElementInstance> getElementInstances(String idGuide)
	    throws InternalErrorException, GuideNotFoundException{
	return getGuideManager().getElementInstances(idGuide);
    }
    
    public GuideManager getGuideManager() throws InternalErrorException{
	if (_guideManager==null){
	    _guideManager = new GuideManager(GenericGuideFactory.getDAO().searchAll());
	}
	return _guideManager;
    }

    public void setGuideManager(GuideManager guideManager)
	    throws InternalErrorException {
	_guideManager = guideManager;
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