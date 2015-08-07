package se.cambio.cds.gdl.converters.drools;

import org.joda.time.DateTime;
import org.junit.Before;
import org.junit.runner.RunWith;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import se.cambio.cds.controller.cds.CDSManager;
import se.cambio.cds.controller.guide.GuideManager;
import se.cambio.cds.controller.session.data.Guides;
import se.cambio.cds.model.facade.execution.drools.DroolsRuleEngineFacadeDelegate;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.Domains;
import se.cambio.cds.util.ElementInstanceCollection;
import se.cambio.cm.model.configuration.CmPersistenceConfig;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.PatientNotFoundException;

import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.List;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = CmPersistenceConfig.class)
@ActiveProfiles({"cm-admin-plain-service", "terminology-plain-service", "cm-admin-file-dao"})
public abstract class GDLTestCase {

    @Value("classpath:/archetypes")
    Resource archetypesResource;

    @Value("classpath:/templates")
    Resource templatesResource;

    @Value("classpath:/terminologies")
    Resource terminologiesResource;

    @Value("classpath:/guides")
    Resource guidelinesResource;

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
    public static String BIRTHDATE_DATE_ELEMENT_ID = "openEHR-EHR-OBSERVATION.basic_demographic.v1/data[at0001]/events[at0002]/data[at0003]/items[at0008]";
    public static String GENDER_ELEMENT_ID = "openEHR-EHR-OBSERVATION.basic_demographic.v1/data[at0001]/events[at0002]/data[at0003]/items[at0004]";

    public static String CONTACT_ARCHETYPE_ID = "openEHR-EHR-EVALUATION.contact.v1";
    public static String CONTACT_DATE_END_ELEMENT_ID = "openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0004]";

    @Before
    public void initializeCM() throws URISyntaxException, IOException {
        UserConfigurationManager.setCmFolder(UserConfigurationManager.ARCHETYPES_FOLDER_KW, archetypesResource.getFile().getPath());
        UserConfigurationManager.setCmFolder(UserConfigurationManager.TERMINOLOGIES_FOLDER_KW, terminologiesResource.getFile().getPath());
        UserConfigurationManager.setCmFolder(UserConfigurationManager.TEMPLATES_FOLDER_KW, templatesResource.getFile().getPath());
        UserConfigurationManager.setCmFolder(UserConfigurationManager.GUIDES_FOLDER_KW, guidelinesResource.getFile().getPath());
    }

    public static ArchetypeReference generateICD10DiagnosisArchetypeReference(String icd10Code) {
        ArchetypeReference ar = new ArchetypeReference(Domains.EHR_ID, GDLTestCase.DIAGNOSIS_ARCHETYPE_ID, GDLTestCase.DIAGNOSIS_TEMPLATE_ID);
        DataValue dataValue = new DvCodedText(icd10Code, "ICD10", icd10Code);
        new ElementInstance(GDLTestCase.DIAGNOSIS_CODE_ELEMENT_ID, dataValue, ar, null, dataValue != null ? null : OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        dataValue = new DvDateTime(new DateTime().toString());
        new ElementInstance(GDLTestCase.DIAGNOSIS_DATE_ELEMENT_ID, dataValue, ar, null, dataValue != null ? null : OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        return ar;
    }

    public static ArchetypeReference generateBasicDemographicsArchetypeReference(Calendar birthdate, Gender gender) {
        ArchetypeReference ar = new ArchetypeReference(Domains.EHR_ID, GDLTestCase.BASIC_DEMOGRAPHICS_ARCHETYPE_ID, null);
        String localGenderCode = gender == Gender.FEMALE ? "at0006" : "at0005";
        DataValue dataValue = new DvCodedText(localGenderCode, "local", localGenderCode);
        new ElementInstance(GENDER_ELEMENT_ID, dataValue, ar, null, dataValue != null ? null : OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        dataValue = new DvDateTime(new DateTime(birthdate.getTimeInMillis()).toString());
        new ElementInstance(BIRTHDATE_DATE_ELEMENT_ID, dataValue, ar, null, dataValue != null ? null : OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        return ar;
    }


    public static ArchetypeReference generateOngoingMedicationArchetypeReference(String atcCode) {
        ArchetypeReference ar = new ArchetypeReference(Domains.EHR_ID, GDLTestCase.MEDICATION_ARCHETYPE_ID, GDLTestCase.MEDICATION_TEMPLATE_ID);
        DataValue dataValue = new DvCodedText(atcCode, "ATC", atcCode);
        new ElementInstance(GDLTestCase.MEDICATION_CODE_ELEMENT_ID, dataValue, ar, null, dataValue != null ? null : OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        dataValue = new DvDateTime(new DateTime().plus(-64000000L).toString());
        new ElementInstance(GDLTestCase.MEDICATION_DATE_INIT_ELEMENT_ID, dataValue, ar, null, dataValue != null ? null : OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        dataValue = null;
        new ElementInstance(GDLTestCase.MEDICATION_DATE_END_ELEMENT_ID, dataValue, ar, null, dataValue != null ? null : OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        return ar;
    }

    public static ArchetypeReference generateContactArchetypeReference(DateTime dateTime) {
        ArchetypeReference ar = new ArchetypeReference(Domains.EHR_ID, GDLTestCase.CONTACT_ARCHETYPE_ID, null);
        if (dateTime == null) {
            dateTime = new DateTime();
        }
        DataValue dataValue = new DvDateTime(dateTime.toString());
        new ElementInstance(GDLTestCase.CONTACT_DATE_END_ELEMENT_ID, dataValue, ar, null, dataValue != null ? null : OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        return ar;
    }

    public static Collection<ElementInstance> getElementInstances(Collection<ArchetypeReference> ars) {
        Collection<ElementInstance> eis = new ArrayList<ElementInstance>();
        for (ArchetypeReference ar : ars) {
            eis.addAll(ar.getElementInstancesMap().values());
        }
        return eis;
    }

    public static GuideManager generateGuideManager(Collection<String> guideIds) throws InternalErrorException, InstanceNotFoundException {
        Collection<GuideDTO> guideDTOs = Guides.getInstance().getCMElementByIds(guideIds);
        return new GuideManager(guideDTOs);
    }

    public static RuleExecutionResult executeGuides(List<String> guideIds, Collection<ElementInstance> elementInstances) {
        RuleExecutionResult rer = null;
        try {
            StringBuffer guideIdsSB = new StringBuffer();
            DroolsRuleEngineFacadeDelegate droolsREFD = new DroolsRuleEngineFacadeDelegate();
            String prefix = "";
            for (String guideId : guideIds) {
                guideIdsSB.append(prefix + guideId);
                prefix = ", ";
            }
            GuideManager guideManager = generateGuideManager(guideIds);
            Calendar cal = Calendar.getInstance();
            long startTime = System.currentTimeMillis();
            ElementInstanceCollection eic = new ElementInstanceCollection();
            eic.addAll(elementInstances);
            CDSManager.checkForMissingElements(eic, guideManager.getCompleteElementInstanceCollection(), guideManager, cal);
            Collection<ArchetypeReference> archetypeReferences = eic.getAllArchetypeReferences();
            System.out.println("Executing : " + guideIdsSB.toString());
            rer = droolsREFD.execute(null, guideManager.getAllGuidesDTO(), archetypeReferences, cal);
            long execTime = (System.currentTimeMillis() - startTime);
            System.out.println("Executed in: " + execTime + " ms");
            System.out.println("Rules fired: " + rer.getFiredRules().size());
        } catch (InternalErrorException e) {
            e.printStackTrace();
        } catch (PatientNotFoundException e) {
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return rer;
    }

    private static InputStream load(String fileName) throws Exception {
        return StressTest.class.getClassLoader().getResourceAsStream(fileName);
    }

    public enum Gender {
        MALE, FEMALE
    }
}
