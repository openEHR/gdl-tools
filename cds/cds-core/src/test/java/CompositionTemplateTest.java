import org.junit.Before;
import org.junit.Test;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.net.URISyntaxException;
import java.util.Collection;

import static org.junit.Assert.assertEquals;

public class CompositionTemplateTest {

    @Before
    public void configCM() throws URISyntaxException, InternalErrorException {
        UserConfigurationManager.setParameter(UserConfigurationManager.TERMINOLOGIES_FOLDER_KW, CompositionTemplateTest.class.getClassLoader().getResource("terminologies").toURI().getPath());
        UserConfigurationManager.setParameter(UserConfigurationManager.ARCHETYPES_FOLDER_KW, CompositionTemplateTest.class.getClassLoader().getResource("archetypes").toURI().getPath());
        UserConfigurationManager.setParameter(UserConfigurationManager.TEMPLATES_FOLDER_KW, CompositionTemplateTest.class.getClassLoader().getResource("templates").toURI().getPath());
    }

    @Test
    public void shouldDetectProperNumberOfElementsInTemplate(){
        Collection<ArchetypeElementVO> archetypeElementVOCollection = ArchetypeManager.getInstance().getArchetypeElements().getArchetypeElementsVO("openEHR-EHR-COMPOSITION.encounter.v1", "diagnosis_list_test");
        assertEquals(7, archetypeElementVOCollection.size());
    }
}
