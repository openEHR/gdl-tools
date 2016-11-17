package se.cambio.cds.gdl.converters.drools;

import com.google.gson.Gson;
import org.joda.time.DateTime;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.DvCount;
import org.openehr.rm.datatypes.quantity.DvOrdinal;
import org.openehr.rm.datatypes.quantity.DvQuantity;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import se.cambio.cds.controller.cds.CDSManager;
import se.cambio.cds.controller.guide.GuideManager;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstance;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.facade.execution.vo.RuleReference;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.EhrDataFilterManager;
import se.cambio.cds.util.export.CdsGsonBuilderFactory;
import se.cambio.openehr.util.configuration.CdsConfiguration;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.PatientNotFoundException;

import java.util.*;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.Matchers.closeTo;
import static org.junit.Assert.*;
import static org.junit.Assert.assertThat;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = {CdsConfiguration.class})
public class MathFunctionsTest extends GDLTestCase {

    @Autowired
    EhrDataFilterManager ehrDataFilterManager;

    @Autowired
    CDSManager cdsManager;

    public MathFunctionsTest() {
        super();
    }

    @Test
    public void shouldTestPriorityWithSeveralGuidelines() {
        Collection<ElementInstance> elementInstances = new ArrayList<>();
        List<String> guideIds = new ArrayList<>();
        guideIds.add("math_functions_test");
        RuleExecutionResult rer = executeGuides(guideIds, elementInstances);
        assertEquals(7, rer.getFiredRules().size());
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
        DataValue sinDV = mathResultAR.getElementInstancesMap().get("openEHR-EHR-EVALUATION.math_functions_test.v1/data[at0000]/items[at0003]").getDataValue();
        assertThat(((DvQuantity) sinDV).getMagnitude(), closeTo(0.841, 0.001));
        assertThat(sinDV, instanceOf(DvQuantity.class));
        DataValue cosDV = mathResultAR.getElementInstancesMap().get("openEHR-EHR-EVALUATION.math_functions_test.v1/data[at0000]/items[at0004]").getDataValue();
        assertThat(((DvQuantity) cosDV).getMagnitude(), closeTo(0.540, 0.001));
        assertThat(cosDV, instanceOf(DvQuantity.class));
        DataValue sqrtDV = mathResultAR.getElementInstancesMap().get("openEHR-EHR-EVALUATION.math_functions_test.v1/data[at0000]/items[at0005]").getDataValue();
        assertThat(((DvQuantity) sqrtDV).getMagnitude(), closeTo(2.236, 0.001));
        assertThat(sqrtDV, instanceOf(DvQuantity.class));
        DataValue roundDV = mathResultAR.getElementInstancesMap().get("openEHR-EHR-EVALUATION.math_functions_test.v1/data[at0000]/items[at0006]").getDataValue();
        assertThat(((DvQuantity) roundDV).getMagnitude(), closeTo(1.000, 0.001));
        assertThat(roundDV, instanceOf(DvQuantity.class));
        DataValue toDegreesDV = mathResultAR.getElementInstancesMap().get("openEHR-EHR-EVALUATION.math_functions_test.v1/data[at0000]/items[at0007]").getDataValue();
        assertThat(((DvQuantity) toDegreesDV).getMagnitude(), closeTo(57.295, 0.001));
        assertThat(toDegreesDV, instanceOf(DvQuantity.class));
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