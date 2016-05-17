import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import se.cambio.cm.model.util.TemplateElementMap;
import se.cambio.cm.model.util.TemplateMap;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.net.URISyntaxException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class TemplateMapperTest {

    @Before
    public void loadCM() throws InternalErrorException, URISyntaxException {
        UserConfigurationManager.setCmFolder(UserConfigurationManager.TERMINOLOGIES_FOLDER_KW, TemplateMapperTest.class.getClassLoader().getResource("terminologies").toURI().getPath());
        UserConfigurationManager.setCmFolder(UserConfigurationManager.ARCHETYPES_FOLDER_KW, TemplateMapperTest.class.getClassLoader().getResource("archetypes").toURI().getPath());
        UserConfigurationManager.setCmFolder(UserConfigurationManager.TEMPLATES_FOLDER_KW, TemplateMapperTest.class.getClassLoader().getResource("templates").toURI().getPath());
    }

    @Test
    @Ignore
    public void shouldMapTemplate() throws InstanceNotFoundException, InternalErrorException {
        TemplateMap templateMap = ArchetypeManager.getInstance().getTemplates().generateTemplateMap("medication_atc_indicator");
        assertEquals(templateMap.getElementMaps().size(), 38);
    }

    @Test
    public void shouldMapCodedTextAttributes() throws InternalErrorException, InstanceNotFoundException {
        TemplateMap templateMap = ArchetypeManager.getInstance().getArchetypes().generateTemplateMap("openEHR-EHR-OBSERVATION.basic_demographic.v1");
        assertEquals(templateMap.getElementMaps().size(), 8);
        TemplateElementMap templateElementMap = templateMap.getElementMaps().get("gender");
        assertNotNull(templateElementMap);
        assertEquals(templateElementMap.getAttributeMaps().size(), 2);
        templateElementMap = templateMap.getElementMaps().get("event_time");
        assertNotNull(templateElementMap);
        //assertEquals(templateElementMap.getPath(), "/data[at0001]/events[at0002]/time");
        assertEquals(templateElementMap.getPath(), "/data/events/time");  //Short form
    }

    @Test
    public void shouldMapOrdinalAttributes() throws InternalErrorException, InstanceNotFoundException {
        TemplateMap templateMap = ArchetypeManager.getInstance().getArchetypes().generateTemplateMap("openEHR-EHR-OBSERVATION.chadsvas_score.v1");
        assertEquals(templateMap.getElementMaps().size(), 10);
        TemplateElementMap templateElementMap = templateMap.getElementMaps().get("diabetes");
        assertNotNull(templateElementMap);
        assertEquals(templateElementMap.getAttributeMaps().size(), 2);
    }

    @Test
    @Ignore
    public void shouldMapTemplateForStrokeReview() throws InstanceNotFoundException, InternalErrorException {
        TemplateMap templateMap = ArchetypeManager.getInstance().getTemplates().generateTemplateMap("stroke_prevention_treatment_review");
        assertTrue(templateMap.getElementMaps().containsKey("diabetes"));
        assertTrue(templateMap.getElementMaps().containsKey("diabetes1"));
        assertEquals(templateMap.getElementMaps().size(), 30);
    }

    @Test
    public void shouldMapOrdinalsWithSameValue() throws InstanceNotFoundException, InternalErrorException {
        TemplateMap templateMap = ArchetypeManager.getInstance().getArchetypes().generateTemplateMap("openEHR-EHR-OBSERVATION.downton_fall_risk_index.v1");
        assertEquals(templateMap.getElementMaps().size(), 8);
        TemplateElementMap medications = templateMap.getElementMaps().get("medications");
        assertNotNull(medications);
        assertEquals(7, medications.getAttributeMaps().size());
    }
}
