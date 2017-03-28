package se.cambio.cds.gdl.converters.drools;

import org.joda.time.DateTime;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openehr.rm.datatypes.quantity.DvQuantity;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import se.cambio.cds.configuration.CdsCoreConfiguration;
import se.cambio.cds.controller.cds.CDSManager;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.EhrDataFilterManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.number.IsCloseTo.closeTo;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = {CdsCoreConfiguration.class})
public class TimeExpressionGDLTest extends GDLTestCase {

    @Autowired
    EhrDataFilterManager ehrDataFilterManager;

    @Autowired
    CDSManager cdsManager;

    public TimeExpressionGDLTest() {
        super();
    }

    @Test
    public void shouldDetectConflictingPredicates() {
        Calendar date = Calendar.getInstance();
        date.add(Calendar.YEAR, -2);
        ArchetypeReference ar = generateWeightArchetypeReference(date, 78.0);
        Collection<ElementInstance> elementInstances = getElementInstances(Collections.singleton(ar));
        RuleExecutionResult rer = executeGuides(Collections.singletonList("conflicting_predicates"), elementInstances);
        assertEquals(1, rer.getFiredRules().size());
    }

    @Test
    public void shouldEvaluateTimeExpressionCorrectly() throws InstanceNotFoundException, InternalErrorException {
        Collection<ArchetypeReference> ehrArs = new ArrayList<>();
        Calendar date = Calendar.getInstance();
        date.add(Calendar.YEAR, -20);
        ArchetypeReference ar = generateBasicDemographicsArchetypeReference(date, Gender.FEMALE);
        ehrArs.add(ar);

        List<String> guideIds = Collections.singletonList("TRI.v1.test");
        RuleExecutionResult rer = executeGuides(guideIds, getElementInstances(ehrArs));
        assertThat(rer.getFiredRules().size(), equalTo(1));
        assertThat(rer.getArchetypeReferences().size(), equalTo(1));
        ArchetypeReference resultAr = rer.getArchetypeReferences().iterator().next();
        assertThat(resultAr.getIdArchetype(), equalTo("openEHR-EHR-OBSERVATION.timi_risk_index.v1"));
        ElementInstance elementInstance = resultAr.getElementInstancesMap().get("openEHR-EHR-OBSERVATION.timi_risk_index.v1/data[at0001]/events[at0002]/data[at0003]/items[at0005]");
        assertThat(elementInstance, notNullValue());
        assertThat(elementInstance.getDataValue(), instanceOf(DvQuantity.class));
        DvQuantity quantity = (DvQuantity)elementInstance.getDataValue();
        assertThat(quantity.getMagnitude(), closeTo(2.0, 0.01));
    }

    @Test
    public void shouldPerformCorrectSubtractionOfTimeWithCurrentDateTime() throws InstanceNotFoundException, InternalErrorException {
        Collection<ArchetypeReference> ehrArs = new ArrayList<>();
        List<String> guideIds = Collections.singletonList("time_evaluation_test");
        Calendar calendar = new DateTime("2016-10-10T01:01:01.000Z").toGregorianCalendar();
        RuleExecutionResult rer = executeGuides(guideIds, getElementInstances(ehrArs), calendar);
        assertThat(rer.getFiredRules().size(), equalTo(1));
        assertThat(rer.getArchetypeReferences().size(), equalTo(1));

        ArchetypeReference resultAr = rer.getArchetypeReferences().iterator().next();
        assertThat(resultAr.getIdArchetype(), equalTo("openEHR-EHR-OBSERVATION.body_weight.v1"));
        ElementInstance elementInstance = resultAr.getElementInstancesMap().get("openEHR-EHR-OBSERVATION.body_weight.v1/data/events/time");
        assertThat(elementInstance.getDataValue(), instanceOf(DvDateTime.class));
        DvDateTime dateTime = (DvDateTime) elementInstance.getDataValue();
        assertThat(dateTime.getDateTime().toString(), equalTo(new DateTime("2015-10-10T01:01:01.000Z").toString()));
    }


    @Test
    public void shouldPerformCorrectSubtractionOfTimeWithCustomDateTime() throws InstanceNotFoundException, InternalErrorException {
        Collection<ArchetypeReference> ehrArs = new ArrayList<>();
        List<String> guideIds = Collections.singletonList("time_evaluation_test2");
        String evaluationDateTime = "2016-10-10T01:01:01.000Z";
        Calendar calendar = new DateTime(evaluationDateTime).toGregorianCalendar();
        RuleExecutionResult rer = executeGuides(guideIds, getElementInstances(ehrArs), calendar);
        assertThat(rer.getFiredRules().size(), equalTo(2));
        assertThat(rer.getArchetypeReferences().size(), equalTo(1));

        ArchetypeReference resultAr = rer.getArchetypeReferences().iterator().next();
        assertThat(resultAr.getIdArchetype(), equalTo("openEHR-EHR-OBSERVATION.body_weight.v1"));
        ElementInstance elementInstance = resultAr.getElementInstancesMap().get("openEHR-EHR-OBSERVATION.body_weight.v1/data/events/time");
        assertThat(elementInstance.getDataValue(), instanceOf(DvDateTime.class));
        DvDateTime dateTime = (DvDateTime) elementInstance.getDataValue();
        assertThat(dateTime.getDateTime().toString(), equalTo(new DateTime(evaluationDateTime).toString()));
    }

    @Test
    public void shouldPerformCorrectSubtractionOfYears() throws InstanceNotFoundException, InternalErrorException {
        Collection<ArchetypeReference> ehrArs = new ArrayList<>();
        List<String> guideIds = Collections.singletonList("time_evaluation_test3");
        String evaluationDateTime = "2016-10-10T01:01:01.000Z";
        Calendar calendar = new DateTime(evaluationDateTime).toGregorianCalendar();
        RuleExecutionResult rer = executeGuides(guideIds, getElementInstances(ehrArs), calendar);
        assertThat(rer.getFiredRules().size(), equalTo(2));
        assertThat(rer.getArchetypeReferences().size(), equalTo(2));

        ArchetypeReference resultAr = null;
        for (ArchetypeReference ar : rer.getArchetypeReferences()) {
            if (ar.getIdArchetype().equals("openEHR-EHR-OBSERVATION.height.v1")) {
                resultAr = ar;
            }
        }
        assertThat(resultAr, notNullValue());
        ElementInstance elementInstance = resultAr.getElementInstancesMap().get("openEHR-EHR-OBSERVATION.height.v1/data[at0001]/events[at0002]/data[at0003]/items[at0004]");
        assertThat(elementInstance.getDataValue(), instanceOf(DvQuantity.class));
        DvQuantity quantity = (DvQuantity) elementInstance.getDataValue();
        assertThat(quantity.getMagnitude(), equalTo(2.0));
    }

    @Test
    public void shouldPerformCorrectNumberOfYears() throws InstanceNotFoundException, InternalErrorException {
        Collection<ArchetypeReference> ehrArs = new ArrayList<>();
        List<String> guideIds = Collections.singletonList("time_evaluation_test4");
        String evaluationDateTime = "2016-10-10T01:01:01.000Z";
        Calendar calendar = new DateTime(evaluationDateTime).toGregorianCalendar();
        RuleExecutionResult rer = executeGuides(guideIds, getElementInstances(ehrArs), calendar);
        assertThat(rer.getFiredRules().size(), equalTo(2));
        assertThat(rer.getArchetypeReferences().size(), equalTo(2));

        ArchetypeReference resultAr = null;
        for (ArchetypeReference ar : rer.getArchetypeReferences()) {
            if (ar.getIdArchetype().equals("openEHR-EHR-OBSERVATION.height.v1")) {
                resultAr = ar;
            }
        }
        assertThat(resultAr, notNullValue());
        assertThat(resultAr.getIdArchetype(), equalTo("openEHR-EHR-OBSERVATION.height.v1"));
        ElementInstance elementInstance = resultAr.getElementInstancesMap().get("openEHR-EHR-OBSERVATION.height.v1/data[at0001]/events[at0002]/data[at0003]/items[at0004]");
        assertThat(elementInstance.getDataValue(), instanceOf(DvQuantity.class));
        DvQuantity quantity = (DvQuantity) elementInstance.getDataValue();
        assertThat(quantity.getMagnitude(), closeTo(0.5, 0.01));
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