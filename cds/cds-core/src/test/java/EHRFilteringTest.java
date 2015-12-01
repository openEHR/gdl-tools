import org.joda.time.DateTime;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.model.facade.execution.vo.GeneratedArchetypeReference;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstanceBuilder;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.Domains;
import se.cambio.cds.util.EhrDataFilterManager;
import se.cambio.cds.util.GeneratedElementInstanceCollection;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.configuration.CdsConfiguration;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Set;

import static org.junit.Assert.assertEquals;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = CdsConfiguration.class)
public class EHRFilteringTest extends GenericTestBase {

    @Autowired
    EhrDataFilterManager ehrDataFilterManager;

    private Collection<ArchetypeReference> generateArchetypeReference(){
        Collection<ArchetypeReference> archetypeReferences = new ArrayList<ArchetypeReference>();
        ArchetypeReference ar = new ArchetypeReference(Domains.EHR_ID, "openEHR-EHR-EVALUATION.contact.v1", null);
        archetypeReferences.add(ar);
        new ElementInstance(
                "openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0006]",
                new DvCodedText("Inpatient", "local", "at0007"),
                ar, null, null);
        DvDateTime dvDateTime = new DvDateTime("2013-04-05T12:01:00");
        new ElementInstance(
                "openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0003]",
                dvDateTime,
                ar, null, null);

        ar = new ArchetypeReference(Domains.EHR_ID, "openEHR-EHR-EVALUATION.contact.v1", null);
        archetypeReferences.add(ar);
        new ElementInstance(
                "openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0006]",
                new DvCodedText("Inpatient", "local", "at0008"),
                ar, null, null);
        dvDateTime = new DvDateTime("2013-06-05T12:01:00");
        new ElementInstance(
                "openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0003]",
                dvDateTime,
                ar, null, null);
        return archetypeReferences;
    }


    private DateTime getCurrentDateTime() {
        return new DateTime(Calendar.getInstance().getTimeInMillis());
    }

    @Test
    public void shouldFilterUsingOnePredicate(){
        GeneratedElementInstanceCollection geic = new GeneratedElementInstanceCollection();
        GeneratedArchetypeReference gar = new GeneratedArchetypeReference(Domains.EHR_ID, "openEHR-EHR-EVALUATION.contact.v1", null);
        new PredicateGeneratedElementInstanceBuilder()
                .setId("openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0003]")
                .setArchetypeReference(gar)
                .setNullFlavour(OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO)
                .setOperatorKind(OperatorKind.MIN)
                .createPredicateGeneratedElementInstance();
        geic.add(gar);
        Collection<ArchetypeReference> archetypeReferences = generateArchetypeReference();
        Set<ArchetypeReference> archetypeReferenceSet =
                ehrDataFilterManager.filterEHRData ("testEHRId", getCurrentDateTime(), geic.getAllArchetypeReferencesByDomain(Domains.EHR_ID), archetypeReferences);

        assertEquals(1, archetypeReferenceSet.size());
    }

    @Test
    public void shouldNotFilterMultiplePredicatesWithSameAR(){
        GeneratedElementInstanceCollection geic = new GeneratedElementInstanceCollection();
        GeneratedArchetypeReference gar = new GeneratedArchetypeReference(Domains.EHR_ID, "openEHR-EHR-EVALUATION.contact.v1", null);
        new PredicateGeneratedElementInstanceBuilder()
                .setId("openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0006]")
                .setDataValue(new DvCodedText("Inpatient", "local", "at0007"))
                .setArchetypeReference(gar)
                .setOperatorKind(OperatorKind.EQUALITY)
                .createPredicateGeneratedElementInstance();
        new PredicateGeneratedElementInstanceBuilder()
                .setId("openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0003]")
                .setArchetypeReference(gar)
                .setNullFlavour(OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO)
                .setOperatorKind(OperatorKind.MIN)
                .createPredicateGeneratedElementInstance();
        geic.add(gar);
        Collection<ArchetypeReference> archetypeReferences = generateArchetypeReference();
        Set<ArchetypeReference> archetypeReferenceSet =
                ehrDataFilterManager.filterEHRData("testEHRId", getCurrentDateTime(), geic.getAllArchetypeReferencesByDomain(Domains.EHR_ID), archetypeReferences);
        assertEquals(2, archetypeReferenceSet.size());
    }

    @Test
    public void shouldNotFilterMultiplePredicatesWithDifferentAR(){
        GeneratedElementInstanceCollection geic = new GeneratedElementInstanceCollection();
        GeneratedArchetypeReference gar = new GeneratedArchetypeReference(Domains.EHR_ID, "openEHR-EHR-EVALUATION.contact.v1", null);
        new PredicateGeneratedElementInstanceBuilder()
                .setId("openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0006]")
                .setDataValue(new DvCodedText("Inpatient", "local", "at0007"))
                .setArchetypeReference(gar)
                .setOperatorKind(OperatorKind.EQUALITY)
                .createPredicateGeneratedElementInstance();
        geic.add(gar);
        gar = new GeneratedArchetypeReference(Domains.EHR_ID, "openEHR-EHR-EVALUATION.contact.v1", null);
        new PredicateGeneratedElementInstanceBuilder()
                .setId("openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0003]")
                .setArchetypeReference(gar)
                .setNullFlavour(OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO)
                .setOperatorKind(OperatorKind.MIN)
                .createPredicateGeneratedElementInstance();
        geic.add(gar);
        Collection<ArchetypeReference> archetypeReferences = generateArchetypeReference();
        Set<ArchetypeReference> archetypeReferenceSet =
                ehrDataFilterManager.filterEHRData("testEHRId", getCurrentDateTime(), geic.getAllArchetypeReferencesByDomain(Domains.EHR_ID), archetypeReferences);
        assertEquals(2, archetypeReferenceSet.size());
    }
}
