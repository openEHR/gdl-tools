package se.cambio.cds;

import org.junit.Test;
import se.cambio.cds.GenericTestBase;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.controller.session.data.ArchetypeManager;

import java.util.Collection;

import static org.junit.Assert.assertEquals;

public class CompositionTemplateTest extends GenericTestBase {

    @Test
    public void shouldDetectProperNumberOfElementsInTemplate(){
        Collection<ArchetypeElementVO> archetypeElementVOCollection = ArchetypeManager.getInstance().getArchetypeElements().getArchetypeElementsVO("openEHR-EHR-COMPOSITION.encounter.v1", "diagnosis_list_test");
        assertEquals(7, archetypeElementVOCollection.size());
    }
}
