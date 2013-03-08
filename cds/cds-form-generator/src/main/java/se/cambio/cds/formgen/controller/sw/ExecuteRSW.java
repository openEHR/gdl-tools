package se.cambio.cds.formgen.controller.sw;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;

import javax.swing.SwingWorker;

import se.cambio.cds.controller.guide.ElementInstanceCollection;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.controller.guide.PredicateGeneratedElementInstance;
import se.cambio.cds.formgen.controller.FormGeneratorController;
import se.cambio.cds.model.facade.execution.vo.ArchetypeReference;
import se.cambio.cds.model.facade.execution.vo.ElementInstance;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.util.Domains;
import se.cambio.cds.util.handlers.ExceptionHandler;

/**
 * @author iago.corbal
 *
 */
public class ExecuteRSW extends SwingWorker<Object, Object> {

    //private long _executionTime = 0;
    private RuleExecutionResult _result = null;
    private FormGeneratorController _controller = null;
    public ExecuteRSW(FormGeneratorController controller) {
	super();
	_controller = controller;
    }

    public ExecuteRSW(File file) {
	super();
    }
    protected Object doInBackground() {
	_result = executeGuides(_controller);
	return null;
    }


    public static RuleExecutionResult executeGuides(FormGeneratorController controller) {
	//Long executionTime = null;
	try{
	    //Calendar timeStart = Calendar.getInstance();
	    //Execute
	    //timeStart= Calendar.getInstance();
	    Collection<GuideDTO> guides = controller.getGuideManager().getAllGuidesDTO();
	    Collection<ElementInstance> elementInstances = new ArrayList<ElementInstance>();
	    for (ElementInstance elementInstance : controller.getAllElementInstances()) {
		elementInstances.add(elementInstance);
		ArchetypeReference ar = elementInstance.getArchetypeReference();
		if (Domains.CDS_ID.equals(ar.getIdDomain()) && !(elementInstance instanceof PredicateGeneratedElementInstance)){
		    elementInstance.setDataValue(null);
		    elementInstance.setNullFlavour(GuideUtil.NULL_FLAVOUR_VALUE);
		}
	    }
	    ElementInstanceCollection eic = new ElementInstanceCollection();
	    eic.addAll(elementInstances);
	    RuleExecutionResult result = controller.getCDSFacadeDelegate().monitor(
		    null,
		    guides,
		    eic, 
		    controller.getCurrentDate());
	    //executionTime = Calendar.getInstance().getTimeInMillis()-timeStart.getTimeInMillis();
	    return result;
	}catch(Throwable e){
	    ExceptionHandler.handle(e);
	    return null;
	}
    }

    public FormGeneratorController getController(){
	return _controller;
    }
    
    protected void done() {
	_controller.getViewer().setFree();
	_controller.updateResults(_result);
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