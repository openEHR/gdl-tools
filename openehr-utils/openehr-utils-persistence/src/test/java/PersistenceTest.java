import org.junit.Test;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTOBuilder;
import se.cambio.openehr.model.cm.element.dao.GenericCMElementDAO;
import se.cambio.openehr.model.util.CMElementDAOFactory;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;

import static org.junit.Assert.assertEquals;

public class PersistenceTest {

    @Test
    public void shouldAllowRounTripForAllCMElements() throws InternalErrorException, InstanceNotFoundException {
        ArchetypeDTO archetypeDTO = new ArchetypeDTOBuilder().createArchetypeDTO();
        archetypeDTO.setId("testOrderSetId");
        archetypeDTO.setSource("testSrc");
        archetypeDTO.setLastUpdate(Calendar.getInstance().getTime());
        GenericCMElementDAO<ArchetypeDTO> dao = CMElementDAOFactory.getInstance().getDAO(ArchetypeDTO.class);
        dao.insert(archetypeDTO);
        Collection<ArchetypeDTO> archetypeDTOs = dao.searchAll();
        assertEquals(1, archetypeDTOs.size());

        archetypeDTO = archetypeDTOs.iterator().next();
        assertEquals("testSrc", archetypeDTO.getSource());

        archetypeDTO.setSource("testSrc2");
        dao.update(archetypeDTO);
        archetypeDTOs = dao.searchByIds(Collections.singleton("testOrderSetId"));
        assertEquals(1, archetypeDTOs.size());
        archetypeDTO = archetypeDTOs.iterator().next();
        assertEquals("testSrc2", archetypeDTO.getSource());

        dao.remove("testOrderSetId");
        archetypeDTOs = dao.searchAll();
        assertEquals(0, archetypeDTOs.size());
    }
}
