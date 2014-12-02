import org.junit.Before;
import org.junit.Test;
import se.cambio.cds.controller.guide.SimpleGuideManager;
import se.cambio.cds.controller.session.data.Guides;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.model.facade.execution.vo.GeneratedElementInstance;
import se.cambio.cds.model.facade.execution.vo.RuleReference;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.ElementInstanceCollection;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;

import static org.junit.Assert.assertTrue;

public class GTCodeReferenceGenerationTest {

    @Before
    public void initializeCM() throws URISyntaxException {
        //Load KM
        UserConfigurationManager.setParameter(UserConfigurationManager.TERMINOLOGIES_FOLDER_KW, GTCodeReferenceGenerationTest.class.getClassLoader().getResource("terminologies").toURI().getPath());
        UserConfigurationManager.setParameter(UserConfigurationManager.ARCHETYPES_FOLDER_KW, GTCodeReferenceGenerationTest.class.getClassLoader().getResource("archetypes").toURI().getPath());
        UserConfigurationManager.setParameter(UserConfigurationManager.TEMPLATES_FOLDER_KW, GTCodeReferenceGenerationTest.class.getClassLoader().getResource("templates").toURI().getPath());
    }

    @Test
    public void shouldContainGTCodeReferences() throws InstanceNotFoundException, InternalErrorException {
        Collection<Guide> guides = new ArrayList<Guide>();
        Guides guidesManager = new Guides();
        guides.add(guidesManager.getGuide("FH_screening_criteria_for_GP.v1"));
        guides.add(guidesManager.getGuide("FH_screening_criteria_referral.v1"));
        SimpleGuideManager simpleGuideManager = new SimpleGuideManager(guides);
        ElementInstanceCollection eic = simpleGuideManager.getCompleteElementInstanceCollection();
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
