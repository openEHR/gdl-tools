import junit.framework.TestCase;
import org.joda.time.DateTime;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.model.facade.execution.vo.GeneratedArchetypeReference;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstance;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.Domains;
import se.cambio.cds.util.EHRDataFilterUtil;
import se.cambio.cds.util.GeneratedElementInstanceCollection;
import se.cambio.openehr.util.OpenEHRConstUI;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Set;

/**
 * User: Iago.Corbal
 * Date: 2014-08-11
 * Time: 15:31
 */
public class EHRFilteringTest extends TestCase {

    public Collection<ElementInstance> generateElementInstances(){
        Collection<ElementInstance> elementInstances = new ArrayList<ElementInstance>();
        ArchetypeReference ar = new ArchetypeReference(Domains.EHR_ID, "openEHR-EHR-EVALUATION.contact.v1", null);
        elementInstances.add(new ElementInstance("openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0006]", new DvCodedText("Inpatient", "local", "at0007"), ar, null, null));
        DvDateTime dvDateTime = new DvDateTime("2013-04-05T12:01:00");
        elementInstances.add(new ElementInstance("openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0003]", dvDateTime, ar, null, null));
        ar = new ArchetypeReference(Domains.EHR_ID, "openEHR-EHR-EVALUATION.contact.v1", null);
        elementInstances.add(new ElementInstance("openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0006]", new DvCodedText("Inpatient", "local", "at0007"), ar, null, null));
        dvDateTime = new DvDateTime("2013-06-05T12:01:00");
        elementInstances.add(new ElementInstance("openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0003]", dvDateTime, ar, null, null));
        return elementInstances;
    }


    public void testEHRFilterWithOnePredicate(){
        GeneratedElementInstanceCollection geic = new GeneratedElementInstanceCollection();
        GeneratedArchetypeReference gar = new GeneratedArchetypeReference(Domains.EHR_ID, "openEHR-EHR-EVALUATION.contact.v1", null);
        new PredicateGeneratedElementInstance("openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0006]", new DvCodedText("Inpatient", "local", "at0007"), gar, null, null, OperatorKind.EQUALITY);
        geic.add(gar);
        DateTime dateTime = new DateTime(Calendar.getInstance().getTimeInMillis());
        Collection<ElementInstance> elementInstances = generateElementInstances();
        Set<ArchetypeReference> archetypeReferenceSet = EHRDataFilterUtil.filterEHRData ("testEHRId", dateTime, geic.getAllArchetypeReferencesByDomain(Domains.EHR_ID), elementInstances);
        assertEquals(1, archetypeReferenceSet.size());
    }

    public void testEHRFilterWithMultiplePredicates(){
        GeneratedElementInstanceCollection geic = new GeneratedElementInstanceCollection();
        GeneratedArchetypeReference gar = new GeneratedArchetypeReference(Domains.EHR_ID, "openEHR-EHR-EVALUATION.contact.v1", null);
        new PredicateGeneratedElementInstance("openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0006]", new DvCodedText("Inpatient", "local", "at0007"), gar, null, null, OperatorKind.EQUALITY);
        new PredicateGeneratedElementInstance("openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0003]", null, gar, null, OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO, OperatorKind.MIN);
        geic.add(gar);
        DateTime dateTime = new DateTime(Calendar.getInstance().getTimeInMillis());
        Collection<ElementInstance> elementInstances = generateElementInstances();
        Set<ArchetypeReference> archetypeReferenceSet = EHRDataFilterUtil.filterEHRData ("testEHRId", dateTime, geic.getAllArchetypeReferencesByDomain(Domains.EHR_ID), elementInstances);
        assertEquals(2, archetypeReferenceSet.size());

        geic = new GeneratedElementInstanceCollection();
        gar = new GeneratedArchetypeReference(Domains.EHR_ID, "openEHR-EHR-EVALUATION.contact.v1", null);
        new PredicateGeneratedElementInstance("openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0006]", new DvCodedText("Inpatient", "local", "at0007"), gar, null, null, OperatorKind.EQUALITY);
        geic.add(gar);
        gar = new GeneratedArchetypeReference(Domains.EHR_ID, "openEHR-EHR-EVALUATION.contact.v1", null);
        new PredicateGeneratedElementInstance("openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0003]", null, gar, null, OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO, OperatorKind.MIN);
        geic.add(gar);
        archetypeReferenceSet = EHRDataFilterUtil.filterEHRData ("testEHRId", dateTime, geic.getAllArchetypeReferencesByDomain(Domains.EHR_ID), elementInstances);
        assertEquals(2, archetypeReferenceSet.size());
    }

}
