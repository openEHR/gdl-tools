import junit.framework.TestCase;
import se.cambio.cds.controller.cds.CDSManager;
import se.cambio.cds.model.facade.execution.vo.GeneratedArchetypeReference;
import se.cambio.cds.model.facade.execution.vo.GeneratedElementInstance;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.util.Domains;
import se.cambio.cds.util.GeneratedElementInstanceCollection;
import se.cambio.openehr.util.OpenEHRConstUI;

import java.util.Collection;

/**
 * User: Iago.Corbal
 * Date: 2014-07-09
 * Time: 12:37
 */
public class CDSManagerTest extends TestCase {
    public void testEHRQuery(){
        GeneratedElementInstanceCollection geic = new GeneratedElementInstanceCollection();
        GeneratedArchetypeReference gar = new GeneratedArchetypeReference(Domains.EHR_ID, "openEHR-EHR-EVALUATION.contact.v1", null);
        new GeneratedElementInstance("openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0006]", null, gar, null, OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        geic.add(gar);
        gar = new GeneratedArchetypeReference(Domains.EHR_ID, "openEHR-EHR-EVALUATION.contact.v1", null);
        new GeneratedElementInstance("openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0004]", null, gar, null, OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        geic.add(gar);
        Collection<ArchetypeReference> ars = CDSManager.getEHRArchetypeReferences(geic);
        assertEquals(1, ars.size()); //Compact
        ArchetypeReference ar = ars.iterator().next();
        assertNotNull(ar.getElementInstancesMap().get("openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0003]")); //Add event time paths
        assertNotNull(ar.getElementInstancesMap().get("openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0004]"));
        assertNotNull(ar.getElementInstancesMap().get("openEHR-EHR-EVALUATION.contact.v1/data[at0001]/items[at0006]"));
    }
}
