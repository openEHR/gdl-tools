package se.cambio.cds.formgen.controller.sw;

import se.cambio.cds.controller.cds.CdsDataManager;
import se.cambio.cds.controller.guide.GuideManager;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.formgen.controller.FormGeneratorController;
import se.cambio.cds.model.facade.execution.delegate.RuleEngineService;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstance;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.Domains;
import se.cambio.cm.model.guide.dto.GuideDTO;

import javax.swing.*;
import java.util.*;

public class ExecuteRSW extends SwingWorker<Object, Object> {

    private RuleExecutionResult result = null;
    private FormGeneratorController controller = null;
    private final CdsDataManager cdsDataManager;
    private RuleEngineService ruleEngineService = null;

    public ExecuteRSW(FormGeneratorController controller,
                      CdsDataManager cdsDataManager,
                      RuleEngineService ruleEngineService) {
        super();
        this.controller = controller;
        this.cdsDataManager = cdsDataManager;
        this.ruleEngineService = ruleEngineService;
    }

    protected Object doInBackground() {
        result = executeGuides(controller);
        return null;
    }


    private RuleExecutionResult executeGuides(FormGeneratorController controller) {
        Collection<String> guideIds = controller.getGuideManager().getAllGuideIds();
        Set<ArchetypeReference> archetypeReferences = new HashSet<>();
        for (ElementInstance elementInstance : controller.getAllElementInstances()) {
            ArchetypeReference ar = elementInstance.getArchetypeReference();
            archetypeReferences.add(ar);
            if (Domains.CDS_ID.equals(ar.getIdDomain()) && !(elementInstance instanceof PredicateGeneratedElementInstance)) {
                elementInstance.setDataValue(null);
                elementInstance.setNullFlavour(GuideUtil.NULL_FLAVOUR_CODE_NO_INFO);
            }
        }

        List<GuideDTO> guideDTOs = Collections.singletonList(controller.getGuideDTO());
        Calendar currentDateTime = controller.getCurrentDate();
        if (currentDateTime == null) {
            currentDateTime = Calendar.getInstance();
        }
        GuideManager guideManager = new GuideManager(guideDTOs, this.controller.getElementInstanceCollectionManager());
        Collection<ArchetypeReference> ehrArchetypeReferences =
                cdsDataManager.getArchetypeReferences(null, guideIds, archetypeReferences, guideManager, currentDateTime);
        return ruleEngineService.execute(null, guideDTOs, ehrArchetypeReferences, currentDateTime);
    }

    public FormGeneratorController getController() {
        return controller;
    }

    protected void done() {
        if (isCancelled()) {
            ruleEngineService.cancelExecution();
        }
        controller.getViewer().setFree();
        controller.updateResults(result);

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