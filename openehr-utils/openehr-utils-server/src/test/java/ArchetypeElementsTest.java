import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import se.cambio.cm.model.configuration.CmPersistenceConfig;
import se.cambio.openehr.controller.session.data.ArchetypeElements;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.controller.session.data.Clusters;
import se.cambio.openehr.controller.session.data.CodedTexts;
import se.cambio.openehr.controller.session.data.Ordinals;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.IOException;
import java.net.URISyntaxException;

import static org.junit.Assert.assertTrue;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = CmPersistenceConfig.class)
@ActiveProfiles({"cm-admin-plain-service", "terminology-plain-service", "cm-admin-file-dao"})
public class ArchetypeElementsTest {

    @Value("classpath:/archetypes")
    Resource archetypesResource;

    @Value("classpath:/templates")
    Resource templatesResource;

    @Value("classpath:/terminologies")
    Resource terminologiesResource;

    @Before
    public void loadCM() throws InternalErrorException, URISyntaxException, IOException {
        UserConfigurationManager.setCmFolder(UserConfigurationManager.TERMINOLOGIES_FOLDER_KW, terminologiesResource.getFile().getPath());
        UserConfigurationManager.setCmFolder(UserConfigurationManager.ARCHETYPES_FOLDER_KW, archetypesResource.getFile().getPath());
        UserConfigurationManager.setCmFolder(UserConfigurationManager.TEMPLATES_FOLDER_KW, templatesResource.getFile().getPath());
    }

    @Test
    public void testArchetypeElementsLanguages(){
        ArchetypeManager archetypeManager = ArchetypeManager.getInstance();
        ArchetypeElements archetypeElements = archetypeManager.getArchetypeElements();
        Ordinals ordinals = archetypeManager.getOrdinals();
        CodedTexts codedTexts = archetypeManager.getCodedTexts();
        Clusters clusters = archetypeManager.getClusters();

        String text = archetypeElements.getText(null,"openEHR-EHR-OBSERVATION.chadsvas_score.v1/data[at0002]/events[at0003]/data[at0001]/items[at0026]","sv");
        assertTrue(text.equals("Hjärtsvikt/VK-dysfunktion"));

        text = ordinals.getText(null, "openEHR-EHR-OBSERVATION.chadsvas_score.v1/data[at0002]/events[at0003]/data[at0001]/items[at0026]", "at0027", "sv");
        assertTrue(text.equals("Finns ej"));

        text = codedTexts.getText(null, "openEHR-EHR-OBSERVATION.basic_demographic.v1/data[at0001]/events[at0002]/data[at0003]/items[at0004]", "at0006", "sv");
        assertTrue(text.equals("Kvinna"));

        text = clusters.getText("medication_atc_indicator", "openEHR-EHR-INSTRUCTION.medication.v1/activities[at0001]/description[openEHR-EHR-ITEM_TREE.medication.v1]/items[at0033]", "sv");
        assertTrue(text.equals("Dose")); //No translation to swedish

        text = archetypeElements.getText("diagnosis_icd10", "openEHR-EHR-EVALUATION.problem-diagnosis.v1/data[at0001]/items[at0002.1]", "sv");
        assertTrue(text.equals("Diagnosis"));
    }
}
