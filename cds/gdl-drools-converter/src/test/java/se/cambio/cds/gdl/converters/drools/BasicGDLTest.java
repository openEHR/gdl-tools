package se.cambio.cds.gdl.converters.drools;

import org.joda.time.DateTime;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.DvCount;
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
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.PatientNotFoundException;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;

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
        Collection<ElementInstance> elementInstances = new ArrayList<ElementInstance>();
        ArchetypeReference ar = new ArchetypeReference(Domains.EHR_ID, GDLTestCase.MEDICATION_ARCHETYPE_ID, GDLTestCase.MEDICATION_TEMPLATE_ID);
        DataValue dataValue = new DvCodedText("N02AX02", "ATC", "N02AX02");
        ElementInstance eiCode = new ElementInstance(GDLTestCase.MEDICATION_CODE_ELEMENT_ID, dataValue, ar, null, dataValue!=null?null: OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        dataValue = new DvDateTime(new DateTime().plus(-64000000L).toString());
        ElementInstance eiInitDate = new ElementInstance(GDLTestCase.MEDICATION_DATE_INIT_ELEMENT_ID, dataValue, ar, null, dataValue!=null?null: OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        dataValue = null;//new DvDateTime(new DateTime().plus(6400000L).toString());
        ElementInstance eiEndDate = new ElementInstance(GDLTestCase.MEDICATION_DATE_END_ELEMENT_ID, dataValue, ar, null, dataValue!=null?null: OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        elementInstances.add(eiCode);
        elementInstances.add(eiInitDate);
        elementInstances.add(eiEndDate);

        ar = new ArchetypeReference(Domains.EHR_ID, GDLTestCase.MEDICATION_ARCHETYPE_ID, GDLTestCase.MEDICATION_TEMPLATE_ID);
        dataValue = new DvCodedText("A10BX03", "ATC", "A10BX03");
        eiCode = new ElementInstance(GDLTestCase.MEDICATION_CODE_ELEMENT_ID, dataValue, ar, null, dataValue!=null?null: OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        dataValue = new DvDateTime(new DateTime().plus(-64000000L).toString());
        eiInitDate = new ElementInstance(GDLTestCase.MEDICATION_DATE_INIT_ELEMENT_ID, dataValue, ar, null, dataValue!=null?null: OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        dataValue = null;//new DvDateTime(new DateTime().plus(6400000L).toString());
        eiEndDate = new ElementInstance(GDLTestCase.MEDICATION_DATE_END_ELEMENT_ID, dataValue, ar, null, dataValue!=null?null: OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        elementInstances.add(eiCode);
        elementInstances.add(eiInitDate);
        elementInstances.add(eiEndDate);

        DroolsRuleExecutionFacadeDelegate droolsREFD = new DroolsRuleExecutionFacadeDelegate();
        Collection<GuideDTO> guideDTOs = new ArrayList<GuideDTO>();
        try {
            guideDTOs.add(GDLTestCase.parse("guides/count_test.v1"));
            GuideManager guideManager = new GuideManager(guideDTOs);
            Calendar cal = Calendar.getInstance();
            long startTime = System.currentTimeMillis();
            ElementInstanceCollection eic = new ElementInstanceCollection();
            eic.addAll(elementInstances);
            CDSManager.checkForMissingElements(eic, guideManager.getCompleteElementInstanceCollection(), guideManager, cal);
            Collection<ElementInstance> eis = eic.getAllElementInstances();
            RuleExecutionResult rer = droolsREFD.execute(null, guideDTOs, eis, cal);
            assertEquals(1, rer.getArchetypeReferences().size());
            ArchetypeReference arResult = rer.getArchetypeReferences().iterator().next();
            assertEquals(1, arResult.getElementInstancesMap().size());
            ElementInstance ei = arResult.getElementInstancesMap().get("openEHR-EHR-OBSERVATION.chadsvas_score.v1/data[at0002]/events[at0003]/data[at0001]/items[at0099]");
            assertNotNull(ei);
            assertTrue(ei.getDataValue() instanceof DvCount);
            assertEquals(2, ((DvCount)ei.getDataValue()).getMagnitude().intValue());
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
    }
}
