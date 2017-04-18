package se.cambio.cds;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import se.cambio.cds.controller.guide.SimpleGuideManager;
import se.cambio.cds.controller.session.data.Guides;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.model.facade.execution.vo.GeneratedElementInstance;
import se.cambio.cds.model.facade.execution.vo.RuleReference;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.ElementInstanceCollection;
import se.cambio.cds.util.ElementInstanceCollectionManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.ArrayList;
import java.util.Collection;

import static org.junit.Assert.assertTrue;


public class GTCodeReferenceGenerationTest extends GenericTestBase {

    @Autowired
    private Guides guideManager;

    @Autowired
    private ElementInstanceCollectionManager elementInstanceCollectionManager;

    @Test
    public void shouldContainGTCodeReferences() throws InstanceNotFoundException, InternalErrorException {
        Collection<Guide> guides = new ArrayList<>();
        guides.add(guideManager.getGuide("FH_screening_criteria_for_GP.v1"));
        guides.add(guideManager.getGuide("FH_screening_criteria_referral.v1"));
        ElementInstanceCollection eic = new SimpleGuideManager(guides, elementInstanceCollectionManager).getCompleteElementInstanceCollection();
        Collection<ArchetypeReference> archetypeReferences = eic.getArchetypeReferences(new ArchetypeReference("EHR", "openEHR-EHR-EVALUATION.family_history.v1", null));
        boolean gtCodeFound = false;
        for (ArchetypeReference archetypeReference: archetypeReferences){
            ElementInstance elementInstance = archetypeReference.getElementInstancesMap().get("openEHR-EHR-EVALUATION.family_history.v1/data[at0001]/items[openEHR-EHR-CLUSTER.genetic_relative-familial_hypercholesteroloemia.v2]/items[at0041]");
            if (elementInstance instanceof GeneratedElementInstance) {
                GeneratedElementInstance gei = (GeneratedElementInstance)elementInstance;
                for(RuleReference ruleReference: gei.getRuleReferences()){
                    if (ruleReference.getGTCode().equals("gt0014")){
                        gtCodeFound = true;
                    }
                }
            }
        }
        assertTrue(gtCodeFound);
    }
}
