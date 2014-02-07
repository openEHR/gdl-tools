package se.cambio.cds.formgen.controller.sw;

import se.cambio.cds.controller.cds.CDSManager;
import se.cambio.cds.controller.guide.GuideManager;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.formgen.controller.FormGeneratorController;
import se.cambio.cds.model.facade.execution.delegate.RuleExecutionFacadeDelegate;
import se.cambio.cds.model.facade.execution.delegate.RuleExecutionFacadeDelegateFactory;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstance;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.Domains;
import se.cambio.openehr.util.ExceptionHandler;

import javax.swing.*;
import java.io.File;
import java.util.*;

/**
 * @author iago.corbal
 *
 */
public class ExecuteRSW extends SwingWorker<Object, Object> {

    //private long _executionTime = 0;
    private RuleExecutionResult _result = null;
    private FormGeneratorController _controller = null;
    private RuleExecutionFacadeDelegate _refd = null;
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


    public RuleExecutionResult executeGuides(FormGeneratorController controller) {
        //Long executionTime = null;
        try{
            //Calendar timeStart = Calendar.getInstance();
            //Execute
            //timeStart= Calendar.getInstance();
            Collection<String> guideIds = controller.getGuideManager().getAllGuideIds();
            Set<ArchetypeReference> archetypeReferences = new HashSet<ArchetypeReference>();
            for (ElementInstance elementInstance : controller.getAllElementInstances()) {
                ArchetypeReference ar = elementInstance.getArchetypeReference();
                archetypeReferences.add(ar);
                if (Domains.CDS_ID.equals(ar.getIdDomain()) && !(elementInstance instanceof PredicateGeneratedElementInstance)){
                    elementInstance.setDataValue(null);
                    elementInstance.setNullFlavour(GuideUtil.NULL_FLAVOUR_CODE_NO_INFO);
                }
            }

            Collection<GuideDTO> guideDTOs = Collections.singleton(controller.getGuideDTO());
            Calendar currentDateTime = controller.getCurrentDate();
            if (currentDateTime==null){
                currentDateTime = Calendar.getInstance();
            }
            GuideManager guideManager = new GuideManager(guideDTOs);
            Collection<ElementInstance> elementInstances =
                    CDSManager.getElementInstances(null, guideIds, archetypeReferences, guideManager, currentDateTime);
            _refd = RuleExecutionFacadeDelegateFactory.getDelegate();
            RuleExecutionResult result = _refd.execute(null, guideDTOs, elementInstances, currentDateTime);
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
        if (isCancelled()){
            _refd.cancelExecution();
        }
        _controller.getViewer().setFree();
        _controller.updateResults(_result);

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