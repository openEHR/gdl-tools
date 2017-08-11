import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.testng.AbstractTestNGSpringContextTests;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import se.cambio.cm.model.facade.configuration.ClinicalModelsConfiguration;
import se.cambio.cm.model.util.TemplateElementMap;
import se.cambio.cm.model.util.TemplateMap;
import se.cambio.openehr.controller.session.configuration.ClinicalModelsCacheConfiguration;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.net.URISyntaxException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

@ContextConfiguration(classes = ClinicalModelsCacheConfiguration.class)
@ActiveProfiles({ "cm-admin-file-dao"})
public class TemplateMapperTest extends AbstractTestNGSpringContextTests {

    @Autowired
    private ArchetypeManager archetypeManager;

    @Autowired
    UserConfigurationManager userConfigurationManager;

    @BeforeClass
    public void loadCM() throws URISyntaxException {
        userConfigurationManager.setTerminologiesFolderPath(TemplateMapperTest.class.getClassLoader().getResource("terminologies").toURI().getPath());
        userConfigurationManager.setArchetypesFolderPath(TemplateMapperTest.class.getClassLoader().getResource("archetypes").toURI().getPath());
        userConfigurationManager.setTemplatesFolderPath(TemplateMapperTest.class.getClassLoader().getResource("templates").toURI().getPath());
    }

    @Test
    public void shouldMapTemplate() throws InstanceNotFoundException, InternalErrorException {
        TemplateMap templateMap = archetypeManager.getTemplates().generateTemplateMap("medication_atc_indicator");
        assertEquals(38, templateMap.getElementMaps().size());
    }

    @Test
    public void shouldMapCodedTextAttributes() throws InternalErrorException, InstanceNotFoundException {
        TemplateMap templateMap = archetypeManager.getArchetypes().generateTemplateMap("openEHR-EHR-OBSERVATION.basic_demographic.v1");
        assertEquals(8, templateMap.getElementMaps().size());
        TemplateElementMap templateElementMap = templateMap.getElementMaps().get("gender");
        assertNotNull(templateElementMap);
        assertEquals(2, templateElementMap.getAttributeMaps().size());
        templateElementMap = templateMap.getElementMaps().get("event_time");
        assertNotNull(templateElementMap);
        //assertEquals(templateElementMap.getPath(), "/data[at0001]/events[at0002]/time");
        assertEquals(templateElementMap.getPath(), "/data/events/time");  //Short form
    }

    @Test
    public void shouldMapOrdinalAttributes() throws InternalErrorException, InstanceNotFoundException {
        TemplateMap templateMap = archetypeManager.getArchetypes().generateTemplateMap("openEHR-EHR-OBSERVATION.chadsvas_score.v1");
        assertEquals(10, templateMap.getElementMaps().size());
        TemplateElementMap templateElementMap = templateMap.getElementMaps().get("diabetes");
        assertNotNull(templateElementMap);
        assertEquals(2, templateElementMap.getAttributeMaps().size());
    }

    @Test
    public void shouldMapTemplateForStrokeReview() throws InstanceNotFoundException, InternalErrorException {
        TemplateMap templateMap = archetypeManager.getTemplates().generateTemplateMap("stroke_prevention_treatment_review");
        assertTrue(templateMap.getElementMaps().containsKey("diabetes"));
        assertTrue(templateMap.getElementMaps().containsKey("diabetes1"));
        assertEquals(30, templateMap.getElementMaps().size());
    }

    @Test
    public void shouldMapOrdinalsWithSameValue() throws InstanceNotFoundException, InternalErrorException {
        TemplateMap templateMap = archetypeManager.getArchetypes().generateTemplateMap("openEHR-EHR-OBSERVATION.downton_fall_risk_index.v1");
        assertEquals(8, templateMap.getElementMaps().size());
        TemplateElementMap medications = templateMap.getElementMaps().get("medications");
        assertNotNull(medications);
        assertEquals(7, medications.getAttributeMaps().size());
    }
}
