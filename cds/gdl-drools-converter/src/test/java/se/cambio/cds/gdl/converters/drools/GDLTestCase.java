package se.cambio.cds.gdl.converters.drools;

import junit.framework.TestCase;
import org.joda.time.DateTime;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.cds.controller.cds.CDSManager;
import se.cambio.cds.controller.guide.GuideManager;
import se.cambio.cds.model.facade.execution.drools.DroolsRuleExecutionFacadeDelegate;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.Domains;
import se.cambio.cds.util.ElementInstanceCollection;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.controller.terminology.session.data.Terminologies;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.PatientNotFoundException;

import java.io.InputStream;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;

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
    public static String BIRTHDATE_DATE_END_ELEMENT_ID = "openEHR-EHR-OBSERVATION.basic_demographic.v1/data[at0001]/events[at0002]/data[at0004]/items[at0008]";

    public static String CONTACT_ARCHETYPE_ID = "openEHR-EHR-EVALUATION.contact.v1";
    public static String CONTACT_DATE_END_ELEMENT_ID = "openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0004]";

    public GDLTestCase(){
        try {
            //Load KM
            UserConfigurationManager.setParameter(UserConfigurationManager.TERMINOLOGIES_FOLDER_KW, StressTest.class.getClassLoader().getResource("terminologies").toURI().getPath());
            Terminologies.loadTerminologies();
            UserConfigurationManager.setParameter(UserConfigurationManager.ARCHETYPES_FOLDER_KW, StressTest.class.getClassLoader().getResource("archetypes").toURI().getPath());
            Archetypes.loadArchetypes();
            UserConfigurationManager.setParameter(UserConfigurationManager.TEMPLATES_FOLDER_KW, StressTest.class.getClassLoader().getResource("templates").toURI().getPath());
            Templates.loadTemplates();
        } catch (InternalErrorException e) {
            e.printStackTrace();
        } catch (URISyntaxException e) {
            e.printStackTrace();
        }
    }


    public static ArchetypeReference generateOngoingMedicationArchetypeReference(String atcCode){
        ArchetypeReference ar = new ArchetypeReference(Domains.EHR_ID, GDLTestCase.MEDICATION_ARCHETYPE_ID, GDLTestCase.MEDICATION_TEMPLATE_ID);
        DataValue dataValue = new DvCodedText(atcCode, "ATC", atcCode);
        new ElementInstance(GDLTestCase.MEDICATION_CODE_ELEMENT_ID, dataValue, ar, null, dataValue!=null?null: OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        dataValue = new DvDateTime(new DateTime().plus(-64000000L).toString());
        new ElementInstance(GDLTestCase.MEDICATION_DATE_INIT_ELEMENT_ID, dataValue, ar, null, dataValue!=null?null: OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        dataValue = null;
        new ElementInstance(GDLTestCase.MEDICATION_DATE_END_ELEMENT_ID, dataValue, ar, null, dataValue!=null?null: OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        return ar;
    }

    public static ArchetypeReference generateContactArchetypeReference(DateTime dateTime){
        ArchetypeReference ar = new ArchetypeReference(Domains.EHR_ID, GDLTestCase.CONTACT_ARCHETYPE_ID, null);
        if (dateTime==null){
            dateTime = new DateTime();
        }
        DataValue dataValue = new DvDateTime(dateTime.toString());
        new ElementInstance(GDLTestCase.CONTACT_DATE_END_ELEMENT_ID, dataValue, ar, null, dataValue!=null?null: OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        return ar;
    }

    public static Collection<ElementInstance> getElementInstances(Collection<ArchetypeReference> ars){
        Collection<ElementInstance> eis = new ArrayList<ElementInstance>();
        for(ArchetypeReference ar: ars){
            eis.addAll(ar.getElementInstancesMap().values());
        }
        return eis;
    }

    public static RuleExecutionResult executeGuides(Collection<String> guideIds, Collection<ElementInstance> elementInstances){
        RuleExecutionResult rer = null;
        try{
            StringBuffer guideIdsSB = new StringBuffer();
            DroolsRuleExecutionFacadeDelegate droolsREFD = new DroolsRuleExecutionFacadeDelegate();
            Collection<GuideDTO> guideDTOs = new ArrayList<GuideDTO>();
            String prefix = "";
            for(String guideId: guideIds){
                guideIdsSB.append(prefix+guideId);
                guideDTOs.add(GDLTestCase.parse("guides/"+guideId));
                prefix = ", ";
            }
            GuideManager guideManager = new GuideManager(guideDTOs);
            Calendar cal = Calendar.getInstance();
            long startTime = System.currentTimeMillis();
            ElementInstanceCollection eic = new ElementInstanceCollection();
            eic.addAll(elementInstances);
            CDSManager.checkForMissingElements(eic, guideManager.getCompleteElementInstanceCollection(), guideManager, cal);
            Collection<ElementInstance> eis = eic.getAllElementInstances();
            System.out.println("Executing : "+guideIdsSB.toString());
            rer = droolsREFD.execute(null, guideDTOs, eis, cal);
            long execTime = (System.currentTimeMillis()-startTime);
            System.out.println("Executed in: "+execTime+" ms");
            System.out.println("Rules fired: "+rer.getFiredRules().size());
        } catch (InternalErrorException e) {
            e.printStackTrace();
        } catch (PatientNotFoundException e) {
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return rer;
    }


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
