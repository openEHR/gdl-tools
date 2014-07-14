package se.cambio.cds.gdl.converters.drools;

import junit.framework.TestCase;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.openehr.util.IOUtils;

import java.io.InputStream;
import java.util.Calendar;

/**
 * User: Iago.Corbal
 * Date: 2014-07-09
 * Time: 12:37
 */
public abstract class GDLTestCase extends TestCase {

    public static String DIAGNOSIS_ARCHETYPE_ID = "openEHR-EHR-EVALUATION.problem-diagnosis.v1";
    public static String DIAGNOSIS_TEMPLATE_ID = "diagnosis_icd10";
    public static String DIAGNOSIS_CODE_ELEMENT_ID = "openEHR-EHR-EVALUATION.problem-diagnosis.v1/data[at0001]/items[at0002.1]";
    public static String DIAGNOSIS_DATE_ELEMENT_ID = "openEHR-EHR-EVALUATION.problem-diagnosis.v1/data[at0001]/items[at0003]";

    public static String MEDICATION_ARCHETYPE_ID = "openEHR-EHR-INSTRUCTION.medication.v1";
    public static String MEDICATION_TEMPLATE_ID = "medication_atc_indicator";
    public static String MEDICATION_CODE_ELEMENT_ID = "openEHR-EHR-INSTRUCTION.medication.v1/activities[at0001]/description[openEHR-EHR-ITEM_TREE.medication.v1]/items[at0012]";
    public static String MEDICATION_DATE_INIT_ELEMENT_ID = "openEHR-EHR-INSTRUCTION.medication.v1/activities[at0001]/description[openEHR-EHR-ITEM_TREE.medication.v1]/items[at0018]/items[at0019]";
    public static String MEDICATION_DATE_END_ELEMENT_ID = "openEHR-EHR-INSTRUCTION.medication.v1/activities[at0001]/description[openEHR-EHR-ITEM_TREE.medication.v1]/items[at0018]/items[at0032]";

    public static String BASIC_DEMOGRAPHICS_ARCHETYPE_ID = "openEHR-EHR-OBSERVATION.basic_demographic.v1";
    public static String BIRTHDATE_DATE_END_ELEMENT_ID = "openEHR-EHR-OBSERVATION.basic_demographic.v1/data[at0001]/events[at0002]/data[at0003]/items[at0008]";


    public static GuideDTO parse(String guideId) throws Exception {
        InputStream is = load(guideId+".gdl");
        String gdlStr = IOUtils.toString(is);
        GuideDTO guideDTO = new GuideDTO(guideId, gdlStr, null, null, true, Calendar.getInstance().getTime());
        DroolsGuideUtil.compileIfNeeded(guideDTO);
        return guideDTO;
    }

    private static InputStream load(String fileName) throws Exception {
        return StressTest.class.getClassLoader().getResourceAsStream(fileName);
    }
}
