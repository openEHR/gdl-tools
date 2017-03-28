package se.cambio.cds.gdl.converters.drools;

import org.joda.time.DateTime;
import org.junit.Before;
import org.junit.runner.RunWith;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.DvQuantity;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import se.cambio.cds.configuration.DroolsConfiguration;
import se.cambio.cds.controller.cds.CDSManager;
import se.cambio.cds.controller.guide.GuideManager;
import se.cambio.cds.controller.session.data.Guides;
import se.cambio.cds.model.facade.execution.drools.DroolsRuleEngineFacadeDelegate;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.Domains;
import se.cambio.cds.util.ElementInstanceCollection;
import se.cambio.cds.util.ElementInstanceCollectionManager;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.List;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = DroolsConfiguration.class)
@ActiveProfiles({"cm-admin-plain-service", "terminology-plain-service", "cm-admin-file-dao"})
public abstract class GDLTestCase {

    @Autowired
    private CDSManager cdsManager;

    @Value("classpath:/archetypes")
    private Resource archetypesResource;

    @Value("classpath:/templates1")
    private Resource templatesResource;

    @Value("classpath:/terminologies")
    private Resource terminologiesResource;

    @Value("classpath:/guides")
    private Resource guidelinesResource;

    @Autowired
    private DroolsRuleEngineFacadeDelegate droolsRuleEngineFacadeDelegate;

    @Autowired
    private Guides guides;

    @Autowired
    private ElementInstanceCollectionManager elementInstanceCollectionManager;

    static String DIAGNOSIS_ARCHETYPE_ID = "openEHR-EHR-EVALUATION.problem-diagnosis.v1";
    static String DIAGNOSIS_TEMPLATE_ID = "diagnosis_icd10";
    static String DIAGNOSIS_CODE_ELEMENT_ID = "openEHR-EHR-EVALUATION.problem-diagnosis.v1/data[at0001]/items[at0002.1]";
    static String DIAGNOSIS_DATE_ELEMENT_ID = "openEHR-EHR-EVALUATION.problem-diagnosis.v1/data[at0001]/items[at0003]";

    static String MEDICATION_ARCHETYPE_ID = "openEHR-EHR-INSTRUCTION.medication.v1";
    static String MEDICATION_TEMPLATE_ID = "medication_atc_indicator";
    static String MEDICATION_CODE_ELEMENT_ID = "openEHR-EHR-INSTRUCTION.medication.v1/activities[at0001]/description[openEHR-EHR-ITEM_TREE.medication.v1]/items[at0012]";
    static String MEDICATION_DATE_INIT_ELEMENT_ID = "openEHR-EHR-INSTRUCTION.medication.v1/activities[at0001]/description[openEHR-EHR-ITEM_TREE.medication.v1]/items[at0018]/items[at0019]";
    static String MEDICATION_DATE_END_ELEMENT_ID = "openEHR-EHR-INSTRUCTION.medication.v1/activities[at0001]/description[openEHR-EHR-ITEM_TREE.medication.v1]/items[at0018]/items[at0032]";

    static String BASIC_DEMOGRAPHICS_ARCHETYPE_ID = "openEHR-EHR-OBSERVATION.basic_demographic.v1";
    static String BIRTHDATE_DATE_ELEMENT_ID = "openEHR-EHR-OBSERVATION.basic_demographic.v1/data[at0001]/events[at0002]/data[at0003]/items[at0008]";
    static String GENDER_ELEMENT_ID = "openEHR-EHR-OBSERVATION.basic_demographic.v1/data[at0001]/events[at0002]/data[at0003]/items[at0004]";

    static String WEIGHT_ARCHETYPE_ID = "openEHR-EHR-OBSERVATION.body_weight.v1";
    static String WEIGHT_ELEMENT_ID = "openEHR-EHR-OBSERVATION.body_weight.v1/data[at0002]/events[at0003]/data[at0001]/items[at0004]";
    static String WEIGHT_EVENT_TIME_ELEMENT_ID = "openEHR-EHR-OBSERVATION.body_weight.v1/data/events/time";


    static String CONTACT_ARCHETYPE_ID = "openEHR-EHR-EVALUATION.contact.v1";
    static String CONTACT_DATE_END_ELEMENT_ID = "openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0004]";
    static String CONTACT_DATE_START_ELEMENT_ID = "openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0003]";
    static String CONTACT_PRIMARY_CARE_VISIT_ELEMENT_ID = "openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0011]";
    static String CONTACT_DOCTOR_VISIT_ELEMENT_ID = "openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0014]";
    static String CONTACT_ADMINISTRATIVE_VISIT_ELEMENT_ID = "openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0015]";

    @Before
    void initializeCM() throws URISyntaxException, IOException {
        UserConfigurationManager.instance().setArchetypesFolderPath(archetypesResource.getFile().getPath());
        UserConfigurationManager.instance().setTerminologiesFolderPath(terminologiesResource.getFile().getPath());
        UserConfigurationManager.instance().setTemplatesFolderPath(templatesResource.getFile().getPath());
        UserConfigurationManager.instance().setGuidelinesFolderPath(guidelinesResource.getFile().getPath());
    }

    public static ArchetypeReference generateICD10DiagnosisArchetypeReference(String icd10Code) {
        ArchetypeReference ar = new ArchetypeReference(Domains.EHR_ID, GDLTestCase.DIAGNOSIS_ARCHETYPE_ID, GDLTestCase.DIAGNOSIS_TEMPLATE_ID);
        DataValue dataValue = new DvCodedText(icd10Code, "ICD10", icd10Code);
        new ElementInstance(GDLTestCase.DIAGNOSIS_CODE_ELEMENT_ID, dataValue, ar, null, null);
        dataValue = new DvDateTime(new DateTime().toString());
        new ElementInstance(GDLTestCase.DIAGNOSIS_DATE_ELEMENT_ID, dataValue, ar, null, null);
        return ar;
    }

    static ArchetypeReference generateBasicDemographicsArchetypeReference(Calendar birthdate, Gender gender) {
        ArchetypeReference ar = new ArchetypeReference(Domains.EHR_ID, GDLTestCase.BASIC_DEMOGRAPHICS_ARCHETYPE_ID, null);
        String localGenderCode = gender == Gender.FEMALE ? "at0006" : "at0005";
        DataValue dataValue = new DvCodedText(localGenderCode, "local", localGenderCode);
        new ElementInstance(GENDER_ELEMENT_ID, dataValue, ar, null, null);
        dataValue = new DvDateTime(new DateTime(birthdate.getTimeInMillis()).toString());
        new ElementInstance(BIRTHDATE_DATE_ELEMENT_ID, dataValue, ar, null, null);
        return ar;
    }

    static ArchetypeReference generateWeightArchetypeReference(Calendar date, Double weightInKg) {
        ArchetypeReference ar = new ArchetypeReference(Domains.EHR_ID, GDLTestCase.WEIGHT_ARCHETYPE_ID, null);
        DataValue dataValue = new DvQuantity("kg", weightInKg, 2);
        new ElementInstance(WEIGHT_ELEMENT_ID, dataValue, ar, null, null);
        dataValue = new DvDateTime(new DateTime(date.getTimeInMillis()).toString());
        new ElementInstance(WEIGHT_EVENT_TIME_ELEMENT_ID, dataValue, ar, null, null);
        return ar;
    }


    static ArchetypeReference generateOngoingMedicationArchetypeReference(String atcCode) {
        ArchetypeReference ar = new ArchetypeReference(Domains.EHR_ID, GDLTestCase.MEDICATION_ARCHETYPE_ID, GDLTestCase.MEDICATION_TEMPLATE_ID);
        DataValue dataValue = new DvCodedText(atcCode, "ATC", atcCode);
        new ElementInstance(GDLTestCase.MEDICATION_CODE_ELEMENT_ID, dataValue, ar, null, null);
        dataValue = new DvDateTime(new DateTime().plus(-64000000L).toString());
        new ElementInstance(GDLTestCase.MEDICATION_DATE_INIT_ELEMENT_ID, dataValue, ar, null, null);
        new ElementInstance(GDLTestCase.MEDICATION_DATE_END_ELEMENT_ID, null, ar, null, OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        return ar;
    }

    static ArchetypeReference generateContactArchetypeReference(DateTime dateTime) {
        ArchetypeReference ar = new ArchetypeReference(Domains.EHR_ID, GDLTestCase.CONTACT_ARCHETYPE_ID, null);
        if (dateTime == null) {
            dateTime = new DateTime();
        }
        DataValue dataValue = new DvDateTime(dateTime.toString());
        new ElementInstance(GDLTestCase.CONTACT_DATE_END_ELEMENT_ID, dataValue, ar, null, null);
        return ar;
    }

    static ArchetypeReference generateContactArchetypeReference(DateTime startTime, DateTime endTime, boolean primaryCare, boolean doctor, boolean administrative) {
        ArchetypeReference ar = generateContactArchetypeReference(endTime);
        DataValue dataValue = new DvDateTime(startTime.toString());
        new ElementInstance(GDLTestCase.CONTACT_DATE_START_ELEMENT_ID, dataValue, ar, null, null);
        dataValue = new DvCodedText(primaryCare ? "present" : "absent", "local", primaryCare ? "at0013" : "at0012");
        new ElementInstance(GDLTestCase.CONTACT_PRIMARY_CARE_VISIT_ELEMENT_ID, dataValue, ar, null, null);
        dataValue = new DvCodedText(doctor ? "present" : "absent", "local", doctor ? "at0013" : "at0012");
        new ElementInstance(GDLTestCase.CONTACT_DOCTOR_VISIT_ELEMENT_ID, dataValue, ar, null, null);
        dataValue = new DvCodedText(administrative ? "present" : "absent", "local", administrative ? "at0013" : "at0012");
        new ElementInstance(GDLTestCase.CONTACT_ADMINISTRATIVE_VISIT_ELEMENT_ID, dataValue, ar, null, null);

        return ar;
    }

    static Collection<ElementInstance> getElementInstances(Collection<ArchetypeReference> ars) {
        Collection<ElementInstance> eis = new ArrayList<>();
        for (ArchetypeReference ar : ars) {
            eis.addAll(ar.getElementInstancesMap().values());
        }
        return eis;
    }

    GuideManager generateGuideManager(Collection<String> guideIds) throws InternalErrorException, InstanceNotFoundException {
        Collection<GuideDTO> guideDTOs = guides.getCMElementByIds(guideIds);
        return new GuideManager(guideDTOs, elementInstanceCollectionManager);
    }

    RuleExecutionResult executeGuides(List<String> guideIds, Collection<ElementInstance> elementInstances) {
        return executeGuides(guideIds, elementInstances, Calendar.getInstance());
    }

    RuleExecutionResult executeGuides(List<String> guideIds, Collection<ElementInstance> elementInstances, Calendar cal) {
        RuleExecutionResult rer = null;
        try {
            StringBuilder guideIdsSB = new StringBuilder();
            String prefix = "";
            for (String guideId : guideIds) {
                guideIdsSB.append(prefix).append(guideId);
                prefix = ", ";
            }
            GuideManager guideManager = generateGuideManager(guideIds);
            long startTime = System.currentTimeMillis();
            ElementInstanceCollection eic = new ElementInstanceCollection(elementInstanceCollectionManager);
            eic.addAll(elementInstances);
            cdsManager.checkForMissingElements(eic, guideManager.getCompleteElementInstanceCollection(), guideManager, cal);
            Collection<ArchetypeReference> archetypeReferences = eic.getAllArchetypeReferences();
            System.out.println("Executing : " + guideIdsSB.toString());
            rer = droolsRuleEngineFacadeDelegate.execute(null, guideManager.getAllGuidesDTO(), archetypeReferences, cal);
            long execTime = (System.currentTimeMillis() - startTime);
            System.out.println("Executed in: " + execTime + " ms");
            System.out.println("Rules fired: " + rer.getFiredRules().size());
        } catch (Exception e) {
            e.printStackTrace();
        }
        return rer;
    }

    public enum Gender {
        FEMALE
    }
}
