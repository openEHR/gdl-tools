import org.junit.Before;
import org.junit.Test;
import org.openehr.rm.datatypes.quantity.ProportionKind;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.cm.model.archetype.vo.CodedTextVO;
import se.cambio.cm.model.util.TemplateElementMap;
import se.cambio.cm.model.util.TemplateMap;
import se.cambio.openehr.controller.session.data.ArchetypeElements;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.net.URISyntaxException;
import java.util.Collection;
import java.util.Map;

import static org.junit.Assert.*;

public class RMElementsTest {

    @Before
    public void loadCM() throws InternalErrorException, URISyntaxException {
        UserConfigurationManager.instance().setArchetypesFolderPath(RMElementsTest.class.getClassLoader().getResource("archetypes").toURI().getPath());
        UserConfigurationManager.instance().setTemplatesFolderPath(RMElementsTest.class.getClassLoader().getResource("templates").toURI().getPath());
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
                archetypeManager.getArchetypeElements().getArchetypeElementsVO("openEHR-TEST_PKG-WHOLE.basic_types.v1.0.0", null);


        assertNotNull(archetypeElementVOs);
        assertFalse(archetypeElementVOs.isEmpty());
    }

    @Test
    public void shouldAllowSpecializationOnDiagnosisStatus() throws InstanceNotFoundException, InternalErrorException {
        ArchetypeManager archetypeManager = ArchetypeManager.getInstance();
        Collection<CodedTextVO> codedTextVOs =
                archetypeManager.getCodedTexts().getCodedTextVOs(null, "openEHR-EHR-EVALUATION.problem-diagnosis.v1.0.0/data[id2]/items[id0.32]");
        assertEquals(2, codedTextVOs.size());
    }

    @Test
    public void shouldParseBasicTypesForOpenEHRCM() throws InstanceNotFoundException, InternalErrorException {
        ArchetypeManager archetypeManager = ArchetypeManager.getInstance();
        ArchetypeElementVO archetypeElementVO =
                archetypeManager.getArchetypeElements().getArchetypeElement(null, "openEHR-EHR-EVALUATION.problem.v1.0.0/data[id2]/items[id3]");
        assertEquals(OpenEHRDataValues.DV_TEXT, archetypeElementVO.getRMType());
        archetypeElementVO =
                archetypeManager.getArchetypeElements().getArchetypeElement(null, "openEHR-EHR-EVALUATION.problem.v1.0.0/data[id2]/items[id4]");
        assertEquals(OpenEHRDataValues.DV_DATE, archetypeElementVO.getRMType());
        archetypeElementVO =
                archetypeManager.getArchetypeElements().getArchetypeElement(null, "openEHR-EHR-EVALUATION.problem.v1.0.0/data[id2]/items[id5]");
        assertEquals(OpenEHRDataValues.DV_DURATION, archetypeElementVO.getRMType());
        archetypeElementVO =
                archetypeManager.getArchetypeElements().getArchetypeElement(null, "openEHR-EHR-EVALUATION.problem.v1.0.0/data[id2]/items[id19]/items[id20]");
        assertEquals(OpenEHRDataValues.DV_QUANTITY, archetypeElementVO.getRMType());
        archetypeElementVO =
                archetypeManager.getArchetypeElements().getArchetypeElement(null, "openEHR-EHR-EVALUATION.problem.v1.0.0/data[id2]/items[id19]/items[id26]");
        assertEquals(OpenEHRDataValues.DV_COUNT, archetypeElementVO.getRMType());
        archetypeElementVO =
                archetypeManager.getArchetypeElements().getArchetypeElement(null, "openEHR-EHR-EVALUATION.problem.v1.0.0/protocol[id33]/items[id34]/items[id36]");
        assertEquals(OpenEHRDataValues.DV_URI, archetypeElementVO.getRMType());
        archetypeElementVO =
                archetypeManager.getArchetypeElements().getArchetypeElement(null, "openEHR-EHR-CLUSTER.device_details.v1.0.0/items[id2]");
        assertEquals(OpenEHRDataValues.DV_IDENTIFIER, archetypeElementVO.getRMType());

        archetypeElementVO =
                archetypeManager.getArchetypeElements().getArchetypeElement(null, "openEHR-EHR-CLUSTER.ambient_oxygen.v1.0.0/items[id53]");
        assertEquals(OpenEHRDataValues.DV_PROPORTION, archetypeElementVO.getRMType());

        Collection<ProportionKind> proportionKinds =
                archetypeManager.getProportionTypes().getProportionTypes(null, "openEHR-EHR-CLUSTER.ambient_oxygen.v1.0.0/items[id53]");
        assertEquals(1, proportionKinds.size());

    }

    @Test
    public void shouldParseBasicTypesForCIMCM() throws InstanceNotFoundException, InternalErrorException {
        ArchetypeManager archetypeManager = ArchetypeManager.getInstance();
        ArchetypeElementVO archetypeElementVO =
                archetypeManager.getArchetypeElements().getArchetypeElement(null, "CIMI-CORE-CLUSTER.finding.v1/item[id3]");
        assertEquals(OpenEHRDataValues.DV_TEXT, archetypeElementVO.getRMType());
        archetypeElementVO =
                archetypeManager.getArchetypeElements().getArchetypeElement(null, "CIMI-CORE-CLUSTER.finding.v1/item[id7]");
        assertEquals(OpenEHRDataValues.DV_COUNT, archetypeElementVO.getRMType());
        archetypeElementVO =
                archetypeManager.getArchetypeElements().getArchetypeElement(null, "CIMI-CORE-CLUSTER.action.v1/item[id4]");
        assertEquals(OpenEHRDataValues.DV_DATE_TIME, archetypeElementVO.getRMType());
        archetypeElementVO =
                archetypeManager.getArchetypeElements().getArchetypeElement(null, "CIMI-CORE-CLUSTER.action.v1/item[id3]");
        assertEquals(OpenEHRDataValues.DV_IDENTIFIER, archetypeElementVO.getRMType());
        archetypeElementVO =
                archetypeManager.getArchetypeElements().getArchetypeElement(null, "CIMI-CORE-CLUSTER.action.v1/item[id2]");
        assertEquals(OpenEHRDataValues.DV_CODED_TEXT, archetypeElementVO.getRMType());
        archetypeElementVO =
                archetypeManager.getArchetypeElements().getArchetypeElement(null, "CIMI-CORE-CLUSTER.action.v1/item[id6]");
        assertEquals(OpenEHRDataValues.DV_DURATION, archetypeElementVO.getRMType());
        archetypeElementVO =
                archetypeManager.getArchetypeElements().getArchetypeElement(null, "CIMI-CORE-ENTRY.heartrate.v1/data[at0.5]/item[at0.0.32]");
        assertEquals(OpenEHRDataValues.DV_CODED_TEXT, archetypeElementVO.getRMType());
        Collection<CodedTextVO> codedTextVOs =
                archetypeManager.getCodedTexts().getCodedTextVOs(null, "CIMI-CORE-ENTRY.heartrate.v1/data[at0.5]/item[at0.0.32]");
        assertEquals(7, codedTextVOs.size());
        archetypeElementVO =
                archetypeManager.getArchetypeElements().getArchetypeElement(null, "CIMI-CORE-ENTRY.heartrate.v1/data[at0.9]/item[at0.16.1]");
        assertEquals(OpenEHRDataValues.DV_QUANTITY, archetypeElementVO.getRMType());
        Collection<String> units =
                archetypeManager.getUnits().getUnits(null, "CIMI-CORE-ENTRY.heartrate.v1/data[at0.9]/item[at0.16.1]");
        assertEquals(1, units.size());
        assertEquals("beats/min", units.iterator().next());
        archetypeElementVO =
                archetypeManager.getArchetypeElements().getArchetypeElement(null, "CIMI-CORE-ENTRY.heartrate.v1/data[at0.9]/item[at0.16.2]");
        assertEquals(OpenEHRDataValues.DV_BOOLEAN, archetypeElementVO.getRMType());
    }
}
