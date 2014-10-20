import org.junit.Before;
import org.junit.Test;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.net.URISyntaxException;
import java.util.Collection;

import static org.junit.Assert.assertEquals;

public class CompositionTemplateTest {

    @Before
    public void loadCM() throws URISyntaxException, InternalErrorException {
        CMUtil.testLoadCM();
    }

    @Test
    public void shouldDetectProperNumberOfElementsInTemplate(){
        Collection<ArchetypeElementVO> archetypeElementVOCollection = ArchetypeManager.getInstance().getArchetypeElements().getArchetypeElementsVO("openEHR-EHR-COMPOSITION.encounter.v1", "diagnosis_list_test");
        assertEquals(7, archetypeElementVOCollection.size());
    }
}
