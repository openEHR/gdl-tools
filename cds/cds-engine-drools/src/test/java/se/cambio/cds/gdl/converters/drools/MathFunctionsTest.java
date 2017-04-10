package se.cambio.cds.gdl.converters.drools;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.DvQuantity;
import org.springframework.beans.factory.annotation.Autowired;
import org.testng.annotations.Test;
import se.cambio.cds.controller.cds.CdsDataManager;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.EhrDataFilterManager;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.Matchers.closeTo;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

public class MathFunctionsTest extends GDLTestCase {

    @Autowired
    EhrDataFilterManager ehrDataFilterManager;

    @Autowired
    CdsDataManager cdsDataManager;

    public MathFunctionsTest() {
        super();
    }

    @Test
    public void shouldTestPriorityWithSeveralGuidelines() {
        Collection<ElementInstance> elementInstances = new ArrayList<>();
        List<String> guideIds = new ArrayList<>();
        guideIds.add("math_functions_test");
        RuleExecutionResult rer = executeGuides(guideIds, elementInstances);
        assertEquals(4, rer.getFiredRules().size());
        ArchetypeReference mathResultAR = null;
        for (ArchetypeReference ar : rer.getArchetypeReferences()) {
            if ("openEHR-EHR-EVALUATION.math_functions_test.v1". equals(ar.getIdArchetype())) {
                mathResultAR = ar;
            }
        }
        assertThat(mathResultAR, notNullValue());
        DataValue logDV = mathResultAR.getElementInstancesMap().get("openEHR-EHR-EVALUATION.math_functions_test.v1/data[at0000]/items[at0001]").getDataValue();
        assertThat(logDV, instanceOf(DvQuantity.class));
        assertThat(((DvQuantity) logDV).getMagnitude(), closeTo(1.098, 0.001));
        DataValue log10DV = mathResultAR.getElementInstancesMap().get("openEHR-EHR-EVALUATION.math_functions_test.v1/data[at0000]/items[at0002]").getDataValue();
        assertThat(((DvQuantity) log10DV).getMagnitude(), closeTo(0.477, 0.001));
        assertThat(log10DV, instanceOf(DvQuantity.class));
        DataValue sqrtDV = mathResultAR.getElementInstancesMap().get("openEHR-EHR-EVALUATION.math_functions_test.v1/data[at0000]/items[at0005]").getDataValue();
        assertThat(((DvQuantity) sqrtDV).getMagnitude(), closeTo(2.236, 0.001));
        assertThat(sqrtDV, instanceOf(DvQuantity.class));
        DataValue roundDV = mathResultAR.getElementInstancesMap().get("openEHR-EHR-EVALUATION.math_functions_test.v1/data[at0000]/items[at0006]").getDataValue();
        assertThat(((DvQuantity) roundDV).getMagnitude(), closeTo(1.000, 0.001));
        assertThat(roundDV, instanceOf(DvQuantity.class));
    }

    @Test
    public void shouldTestConstantE() {
        Collection<ElementInstance> elementInstances = new ArrayList<>();
        List<String> guideIds = new ArrayList<>();
        guideIds.add("test_e_number");
        RuleExecutionResult rer = executeGuides(guideIds, elementInstances);
        assertEquals(1, rer.getFiredRules().size());
        ArchetypeReference mathResultAR = null;
        for (ArchetypeReference ar : rer.getArchetypeReferences()) {
            if ("openEHR-EHR-OBSERVATION.body_weight.v1". equals(ar.getIdArchetype())) {
                mathResultAR = ar;
            }
        }
        assertThat(mathResultAR, notNullValue());
        DataValue eDV = mathResultAR.getElementInstancesMap().get("openEHR-EHR-OBSERVATION.body_weight.v1/data[at0002]/events[at0003]/data[at0001]/items[at0004]").getDataValue();
        assertThat(eDV, instanceOf(DvQuantity.class));
        assertThat(((DvQuantity) eDV).getMagnitude(), closeTo(2.718, 0.001));
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