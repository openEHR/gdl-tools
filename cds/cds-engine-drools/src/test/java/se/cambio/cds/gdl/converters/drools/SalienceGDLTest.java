package se.cambio.cds.gdl.converters.drools;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.facade.execution.vo.RuleReference;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.export.CdsGsonBuilderFactory;
import se.cambio.openehr.util.configuration.CdsConfiguration;

import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.lessThan;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = {CdsConfiguration.class})
public class SalienceGDLTest extends GDLTestCase {

    @Value("classpath:/multiple-guide-salience-test-ar.json")
    Resource testAR;

    public SalienceGDLTest() {
        super();
    }

    @Test
    public void shouldExecuteGuidelinesInCorrectOrder() throws IOException {
        Gson gson = new CdsGsonBuilderFactory().getGsonBuilder().create();
        InputStreamReader streamReader = new InputStreamReader(testAR.getInputStream());
        Type listType = new TypeToken<ArrayList<ArchetypeReference>>() {}.getType();
        Collection<ArchetypeReference> ars = gson.fromJson(streamReader, listType);
        Collection<ElementInstance> elementInstances = getElementInstances(ars);
        List<String> guideIds = new ArrayList<>();
        guideIds.add("CHA2DS2VASc_diagnosis_review.v1");
        guideIds.add("CHA2DS2VASc_Score_calculation.v1.1");
        guideIds.add("Stroke_prevention_alert.v1.1");
        guideIds.add("Stroke_prevention_medication_recommendation.v1");
        guideIds.add("Stroke_prevention_compliance_checking_in_AF.v2");
        RuleExecutionResult rer = executeGuides(guideIds, elementInstances);
        List<RuleReference> firedRules = rer.getFiredRules();
        List<String> firedGuideIds = getCalledGuideIds(firedRules);
        int indexOfLastScoreCalc = firedGuideIds.lastIndexOf("CHA2DS2VASc_Score_calculation.v1.1");
        int indexOfLastMedRec = firedGuideIds.lastIndexOf("Stroke_prevention_compliance_checking_in_AF.v2");
        assertThat(indexOfLastScoreCalc, lessThan(indexOfLastMedRec));
    }

    private List<String> getCalledGuideIds(List<RuleReference> firedRules) {
        List<String> firedGuideIds = new ArrayList<>();
        for (RuleReference ruleReference: firedRules) {
            firedGuideIds.add(ruleReference.getGuideId());
        }
        return firedGuideIds;
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