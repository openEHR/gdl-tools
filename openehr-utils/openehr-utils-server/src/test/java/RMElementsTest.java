import org.junit.Before;
import org.junit.Test;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.cm.model.util.TemplateElementMap;
import se.cambio.cm.model.util.TemplateMap;
import se.cambio.openehr.controller.session.data.ArchetypeElements;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.net.URISyntaxException;
import java.util.Collection;
import java.util.Map;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

public class RMElementsTest {

    @Before
    public void loadCM() throws InternalErrorException, URISyntaxException {
        UserConfigurationManager.setParameter(UserConfigurationManager.ARCHETYPES_FOLDER_KW, RMElementsTest.class.getClassLoader().getResource("archetypes").toURI().getPath());
        UserConfigurationManager.setParameter(UserConfigurationManager.TEMPLATES_FOLDER_KW, RMElementsTest.class.getClassLoader().getResource("templates").toURI().getPath());
    }

    @Test
    public void shouldContainRMElementsForAllEntryArchetypes() throws InstanceNotFoundException, InternalErrorException {
        ArchetypeManager archetypeManager = ArchetypeManager.getInstance();
        Templates templates = archetypeManager.getTemplates();
        ArchetypeElements archetypeElements = archetypeManager.getArchetypeElements();

        String encounter_weight_templateId = "encounter_weight";
        ArchetypeElementVO archetypeElement = archetypeElements.getArchetypeElement(encounter_weight_templateId,"openEHR-EHR-COMPOSITION.encounter.v1/content[openEHR-EHR-OBSERVATION.body_weight.v1]/data/events/time");
        assertNotNull(archetypeElement);

        TemplateMap templateMap = templates.generateTemplateMap(encounter_weight_templateId);
        Map<String, TemplateElementMap> elementMap = templateMap.getElementMaps();
        assertNotNull(elementMap.get("event_time"));
    }

    @Test
    public void shouldParseADLs() throws InstanceNotFoundException, InternalErrorException {
        ArchetypeManager archetypeManager = ArchetypeManager.getInstance();
        Collection<ArchetypeElementVO> archetypeElementVOs =
        //archetypeManager.getArchetypeElements().getArchetypeElementsVO("CIMI-CORE-CLUSTER.leukocytes_in_blood_automated.v1", null);
        //archetypeManager.getArchetypeElements().getArchetypeElementsVO("openEHR-EHR-OBSERVATION.quantity_tuple.v1", null);
        //archetypeManager.getArchetypeElements().getArchetypeElementsVO("openEHR-EHR-EVALUATION.alert.v1", null);
        //archetypeManager.getArchetypeElements().getArchetypeElementsVO("openEHR-EHR-OBSERVATION.ordinal.v1", null);
        archetypeManager.getArchetypeElements().getArchetypeElementsVO("CIMI-CORE-INDIVISIBLE_ENTRY.body_height.v1", null);


        assertNotNull(archetypeElementVOs);
        assertFalse(archetypeElementVOs.isEmpty());
    }
}
