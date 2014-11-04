import org.junit.Before;
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

public class TemplateMapperTest {

    @Before
    public void loadCM() throws InternalErrorException, URISyntaxException {
        UserConfigurationManager.setParameter(UserConfigurationManager.TERMINOLOGIES_FOLDER_KW, TemplateMapperTest.class.getClassLoader().getResource("terminologies").toURI().getPath());
        UserConfigurationManager.setParameter(UserConfigurationManager.ARCHETYPES_FOLDER_KW, TemplateMapperTest.class.getClassLoader().getResource("archetypes").toURI().getPath());
        UserConfigurationManager.setParameter(UserConfigurationManager.TEMPLATES_FOLDER_KW, TemplateMapperTest.class.getClassLoader().getResource("templates").toURI().getPath());
    }

    @Test
    public void shouldMapTemplate() throws InstanceNotFoundException, InternalErrorException {
        TemplateMap templateMap = ArchetypeManager.getInstance().getTemplates().generateTemplateMap("medication_atc_indicator");
        assertEquals(templateMap.getElementMaps().size(), 37);

        templateMap = ArchetypeManager.getInstance().getArchetypes().generateTemplateMap("openEHR-EHR-OBSERVATION.basic_demographic.v1");
        assertEquals(templateMap.getElementMaps().size(), 8);
        TemplateElementMap templateElementMap = templateMap.getElementMaps().get("gender");
        assertNotNull(templateElementMap);
        assertEquals(templateElementMap.getAttributeMaps().size(), 2);
    }
}
