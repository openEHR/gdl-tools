package se.cambio.cds.gdl.converters.drools;

import org.joda.time.DateTime;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.DvCount;
import org.openehr.rm.datatypes.quantity.DvOrdinal;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.cds.controller.cds.CDSManager;
import se.cambio.cds.controller.guide.GuideManager;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstance;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.facade.execution.vo.RuleReference;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.PatientNotFoundException;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;

/**
 * User: Iago.Corbal
 * Date: 2014-07-09
 * Time: 12:37
 */
public class BasicGDLTest extends GDLTestCase {

    public BasicGDLTest(){
        super();
    }

    public void testCount(){
        Collection<ArchetypeReference> ars = new ArrayList<ArchetypeReference>();
        ars.add(generateOngoingMedicationArchetypeReference("A10BX03"));
        ars.add(generateOngoingMedicationArchetypeReference("N02AX02"));
        Collection<ElementInstance> elementInstances = getElementInstances(ars);
        RuleExecutionResult rer = executeGuides(Collections.singleton("count_test.v1"), elementInstances);

        assertEquals(1, rer.getArchetypeReferences().size());
        ArchetypeReference arResult = rer.getArchetypeReferences().iterator().next();
        assertEquals(1, arResult.getElementInstancesMap().size());
        ElementInstance ei = arResult.getElementInstancesMap().get("openEHR-EHR-OBSERVATION.chadsvas_score.v1/data[at0002]/events[at0003]/data[at0001]/items[at0099]");
        assertNotNull(ei);
        assertTrue(ei.getDataValue() instanceof DvCount);
        assertEquals(2, ((DvCount)ei.getDataValue()).getMagnitude().intValue());
    }

    public void testNotExists(){
        ArchetypeReference ar = generateOngoingMedicationArchetypeReference("A10BX03");
        Collection<ElementInstance> elementInstances = getElementInstances(Collections.singleton(ar));
        RuleExecutionResult rer = executeGuides(Collections.singleton("not_exists_test.v1"), elementInstances);
        assertEquals(2, rer.getFiredRules().size());
        assertTrue(rer.getFiredRules().contains(new RuleReference("not_exists_test.v1", "gt0045")));
        assertTrue(rer.getFiredRules().contains(new RuleReference("not_exists_test.v1","gt0039")));
    }

    public void testOrPredicates(){
        ArchetypeReference ar = generateOngoingMedicationArchetypeReference("A01AA01");
        Collection<ElementInstance> elementInstances = getElementInstances(Collections.singleton(ar));
        RuleExecutionResult rer = executeGuides(Collections.singleton("test_or_predicates.v1"), elementInstances);
        assertEquals(4, rer.getFiredRules().size());
        assertTrue(rer.getFiredRules().contains(new RuleReference("test_or_predicates.v1", "gt0002")));
        assertTrue(rer.getFiredRules().contains(new RuleReference("test_or_predicates.v1", "gt0012")));
        assertTrue(rer.getFiredRules().contains(new RuleReference("test_or_predicates.v1","gt0013")));
        assertTrue(rer.getFiredRules().contains(new RuleReference("test_or_predicates.v1","gt0014")));
    }

    public void testCreationAndOrder(){
        ArchetypeReference ar = generateOngoingMedicationArchetypeReference("A01AA01");
        Collection<ElementInstance> elementInstances = getElementInstances(Collections.singleton(ar));
        Collection<String> guideIds = new ArrayList<String>();
        guideIds.add("test_creation_and_order_1");
        guideIds.add("test_creation_and_order_2");
        RuleExecutionResult rer = executeGuides(guideIds, elementInstances);
        assertEquals(4, rer.getFiredRules().size());
        assertTrue(rer.getFiredRules().get(0).equals(new RuleReference("test_creation_and_order_2","gt0002")));
        assertTrue(rer.getFiredRules().get(1).equals(new RuleReference("test_creation_and_order_2","gt0005")));
        assertTrue(rer.getFiredRules().get(2).equals(new RuleReference("test_creation_and_order_1","gt0005")));
        assertTrue(rer.getFiredRules().get(3).equals(new RuleReference("test_creation_and_order_1","gt0002")));
    }

    public void testCDSCount(){
        Collection<ArchetypeReference> ars = new ArrayList<ArchetypeReference>();
        ars.add(generateOngoingMedicationArchetypeReference("A10BX03"));
        ars.add(generateOngoingMedicationArchetypeReference("A10BX02"));
        ars.add(generateOngoingMedicationArchetypeReference("N02AX02"));
        ars.add(generateContactArchetypeReference(new DateTime().plus(-100000)));
        ars.add(generateContactArchetypeReference(new DateTime().plus(100000)));
        ars.add(generateContactArchetypeReference(new DateTime().plus(-200000)));
        Collection<ElementInstance> elementInstances = getElementInstances(ars);
        Collection<String> guideIds = new ArrayList<String>();
        guideIds.add("cds_count");
        RuleExecutionResult rer = executeGuides(guideIds, elementInstances);
        assertEquals(4, rer.getFiredRules().size());
        assertTrue(rer.getFiredRules().get(0).equals(new RuleReference("cds_count","gt0006")));
        assertTrue(rer.getFiredRules().get(1).equals(new RuleReference("cds_count","gt0006")));
        assertTrue(rer.getFiredRules().get(2).equals(new RuleReference("cds_count","gt0006")));
        assertTrue(rer.getFiredRules().get(3).equals(new RuleReference("cds_count","gt0011")));
    }


    public void testMissingElements(){
        Collection<ArchetypeReference> ars = new ArrayList<ArchetypeReference>();
        ArchetypeReference ar = generateOngoingMedicationArchetypeReference("A10BX03");
        ar.getElementInstancesMap().remove(GDLTestCase.MEDICATION_DATE_END_ELEMENT_ID); //Remove end elements
        ars.add(ar);
        ar = generateOngoingMedicationArchetypeReference("A10BX02");
        ar.getElementInstancesMap().remove(GDLTestCase.MEDICATION_DATE_END_ELEMENT_ID);
        ars.add(ar);
        ar = generateOngoingMedicationArchetypeReference("N02AX02");
        ar.getElementInstancesMap().remove(GDLTestCase.MEDICATION_DATE_END_ELEMENT_ID);
        ars.add(ar);
        Collection<String> guideIds = new ArrayList<String>();
        guideIds.add("test_med_definition");
        GuideManager guideManager = generateGuideManager(guideIds);
        try {
            Collection<ElementInstance> elementInstances =
                    CDSManager.getElementInstances(null, guideIds, ars, guideManager, Calendar.getInstance());
            assertEquals(9,elementInstances.size());
        } catch (PatientNotFoundException e) {
            e.printStackTrace();
        } catch (InternalErrorException e) {
            e.printStackTrace();
        }
    }

    public void testMedDefinitionWithPredicates(){
        Collection<ArchetypeReference> ars = new ArrayList<ArchetypeReference>();
        ArchetypeReference ar = generateOngoingMedicationArchetypeReference("A10BX03");
        ar.getElementInstancesMap().remove(GDLTestCase.MEDICATION_DATE_END_ELEMENT_ID); //Remove end elements
        ars.add(ar);
        ar = generateOngoingMedicationArchetypeReference("A01AB06");
        ar.getElementInstancesMap().remove(GDLTestCase.MEDICATION_DATE_END_ELEMENT_ID);
        ars.add(ar);
        ar = generateOngoingMedicationArchetypeReference("N02AX02");
        ar.getElementInstancesMap().remove(GDLTestCase.MEDICATION_DATE_END_ELEMENT_ID);
        ars.add(ar);
        Collection<String> guideIds = new ArrayList<String>();
        guideIds.add("test_med_definition_with_predicates1");
        guideIds.add("test_med_definition_with_predicates2");
        GuideManager guideManager = generateGuideManager(guideIds);
        try {
            Collection<ElementInstance> elementInstances =
                    CDSManager.getElementInstances(null, guideIds, ars, guideManager, Calendar.getInstance());
            assertEquals(18,elementInstances.size());
            boolean predicateForBValuesExists = false;
            boolean predicateForGenericEqualsNullValuesExists = false;
            boolean predicateForMinLastAdministrationExists = false;
            for(ElementInstance elementInstance: elementInstances){
                if (elementInstance instanceof PredicateGeneratedElementInstance){
                    PredicateGeneratedElementInstance pgei = (PredicateGeneratedElementInstance)elementInstance;
                    if (MEDICATION_CODE_ELEMENT_ID.equals(pgei.getId())){
                        if(OperatorKind.INEQUAL.equals(pgei.getOperatorKind())){
                            if (pgei.getDataValue()==null){
                                fail("Predicate medication generic name!=null should not be generated!");
                            }
                        }else if(OperatorKind.EQUALITY.equals(pgei.getOperatorKind())){
                            if (pgei.getDataValue()==null){
                                predicateForGenericEqualsNullValuesExists = true;
                            }
                        }else if(OperatorKind.IS_A.equals(pgei.getOperatorKind())){
                            if (pgei.getDataValue() instanceof DvCodedText){
                                DvCodedText dvCodedText = (DvCodedText)pgei.getDataValue();
                                String code = dvCodedText.getCode();
                                if (code.equals("A01AB06")){
                                    fail("Predicate medication generic name is_a 'A01AB06' should not be generated!");
                                }else if (code.startsWith("B01")){
                                    predicateForBValuesExists =true;
                                }
                            }
                        }
                    }else if (MEDICATION_DATE_INIT_ELEMENT_ID.equals(pgei.getId())){
                        if(OperatorKind.MAX.equals(pgei.getOperatorKind())){
                            fail("Predicate medication generic name!=null should not be generated!");
                        }

                    }else if (MEDICATION_DATE_END_ELEMENT_ID.equals(pgei.getId())){
                        if(OperatorKind.MIN.equals(pgei.getOperatorKind())){
                            predicateForMinLastAdministrationExists=true;
                        }
                    }
                }
            }
            assertTrue(predicateForBValuesExists);
            assertTrue(predicateForGenericEqualsNullValuesExists);
            assertTrue(predicateForMinLastAdministrationExists);
        } catch (PatientNotFoundException e) {
            e.printStackTrace();
        } catch (InternalErrorException e) {
            e.printStackTrace();
        }
    }

    public void testMultipleResults(){
        Collection<ArchetypeReference> ars = new ArrayList<ArchetypeReference>();
        ArchetypeReference ar = generateOngoingMedicationArchetypeReference("A10BX03");
        ar.getElementInstancesMap().remove(GDLTestCase.MEDICATION_DATE_END_ELEMENT_ID); //Remove end elements
        ars.add(ar);
        ar = generateOngoingMedicationArchetypeReference("A10BX02");
        ar.getElementInstancesMap().remove(GDLTestCase.MEDICATION_DATE_END_ELEMENT_ID);
        ars.add(ar);
        ar = generateOngoingMedicationArchetypeReference("N02AX02");
        ars.add(ar);
        ar.getElementInstancesMap().remove(GDLTestCase.MEDICATION_DATE_END_ELEMENT_ID);
        ars.add(generateContactArchetypeReference(new DateTime().plus(100000)));
        Collection<ElementInstance> elementInstances = getElementInstances(ars);
        Collection<String> guideIds = new ArrayList<String>();
        guideIds.add("multiple_results_test");
        RuleExecutionResult rer = executeGuides(guideIds, elementInstances);
        assertEquals(7, rer.getArchetypeReferences().size());
        assertEquals(4, rer.getFiredRules().size());
        assertEquals(4, ars.size());
        for(ArchetypeReference arAux: ars){
            if (GDLTestCase.MEDICATION_ARCHETYPE_ID.equals(arAux.getIdArchetype())){
                assertEquals(3, arAux.getElementInstancesMap().size()); //End date is generated in CDSManager
            }
        }
    }


    public void testCDSInitialization(){
        Collection<ArchetypeReference> ars = new ArrayList<ArchetypeReference>();
        Collection<ElementInstance> elementInstances = getElementInstances(ars);
        Collection<String> guideIds = new ArrayList<String>();
        guideIds.add("test_cds_init1");
        guideIds.add("test_cds_init2");
        RuleExecutionResult rer = executeGuides(guideIds, elementInstances);
        assertEquals(1, rer.getArchetypeReferences().size());
    }

    public void testDateOperation(){
        Collection<ArchetypeReference> ars = new ArrayList<ArchetypeReference>();
        Collection<ElementInstance> elementInstances = getElementInstances(ars);
        Collection<String> guideIds = new ArrayList<String>();
        guideIds.add("test_date_operation");
        RuleExecutionResult rer = executeGuides(guideIds, elementInstances);
        assertEquals(1, rer.getArchetypeReferences().size());
        assertEquals(1, rer.getFiredRules().size());
        ArchetypeReference ar = rer.getArchetypeReferences().iterator().next();
        assertEquals(1, ar.getElementInstancesMap().size());
        ElementInstance contactEndElement = ar.getElementInstancesMap().get(CONTACT_DATE_END_ELEMENT_ID);
        assertNotNull(contactEndElement);
        DataValue dv = contactEndElement.getDataValue();
        assertTrue(dv instanceof DvDateTime);
        assertEquals(Calendar.getInstance().get(Calendar.YEAR)-1,((DvDateTime)dv).getYear());
    }

    /* TODO Disabled until we are able to implement the functionality
    public void testPredicateAsDefaultCDSValues(){
        Collection<ArchetypeReference> ars = new ArrayList<ArchetypeReference>();
        Collection<ElementInstance> elementInstances = getElementInstances(ars);
        Collection<String> guideIds = new ArrayList<String>();
        guideIds.add("test_predicates_as_default_value.v1");
        RuleExecutionResult rer = executeGuides(guideIds, elementInstances);
        assertEquals(5, rer.getFiredRules().size());
    }
    */

    public void testCDSLinking(){
        Collection<ArchetypeReference> ars = new ArrayList<ArchetypeReference>();
        Calendar birthdate = Calendar.getInstance();
        birthdate.add(Calendar.YEAR, -75);
        ars.add(generateBasicDemographicsArchetypeReference(birthdate, Gender.FEMALE));
        ars.add(generateICD10DiagnosisArchetypeReference("I48"));
        Collection<ElementInstance> elementInstances = getElementInstances(ars);
        Collection<String> guideIds = new ArrayList<String>();
        guideIds.add("CHA2DS2VASc_Score_calculation.v1.1");
        guideIds.add("Stroke_risks.v2");
        guideIds.add("CHA2DS2VASc_diagnosis_review.v1");
        guideIds.add("Stroke_prevention_compliance_checking_in_AF.v2");
        guideIds.add("Stroke_prevention_alert.v1.1");
        guideIds.add("Stroke_prevention_medication_recommendation.v1");
        int medicationCount = 0;
        RuleExecutionResult rer = executeGuides(guideIds, elementInstances);
        assertEquals(9, rer.getArchetypeReferences().size());
        assertEquals(11, rer.getFiredRules().size());
        boolean strokeARFound = false;
        for(ArchetypeReference ar: rer.getArchetypeReferences()){
            if (ar.getIdArchetype().equals("openEHR-EHR-OBSERVATION.stroke_risk.v1")){
                strokeARFound = true;
                assertEquals(1, ar.getElementInstancesMap().size());
                ElementInstance strokeRiskElement = ar.getElementInstancesMap().get("openEHR-EHR-OBSERVATION.stroke_risk.v1/data[at0001]/events[at0002]/data[at0003]/items[at0004]");
                assertNotNull(strokeRiskElement);
                DataValue dv = strokeRiskElement.getDataValue();
                assertTrue(dv instanceof DvOrdinal);
                assertEquals(3, ((DvOrdinal)dv).getValue());
            }else if (ar.getIdArchetype().equals("openEHR-EHR-INSTRUCTION.medication.v1")){
                medicationCount++;
            }
        }
        assertTrue(strokeARFound);
        assertEquals(4,medicationCount);
    }
}
