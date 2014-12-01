import org.junit.Test;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.archetype.dto.ArchetypeDTOBuilder;
import se.cambio.cm.model.generic.dao.GenericCMElementDAO;
import se.cambio.cm.model.util.CMElementDAOFactory;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.net.URISyntaxException;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;

import static org.junit.Assert.assertEquals;

public class PersistenceTest {

    @Test
    public void shouldAllowRoundTripForAllCMElements() throws InternalErrorException, InstanceNotFoundException, URISyntaxException {
        UserConfigurationManager.setParameter(UserConfigurationManager.ARCHETYPES_FOLDER_KW, PersistenceTest.class.getClassLoader().getResource("").toURI().getPath());

        ArchetypeDTO archetypeDTO =
                new ArchetypeDTOBuilder()
                        .setId("testArchetypeId")
                        .setFormat("adl")
                        .setSource("testSrc")
                        .setLastUpdate(Calendar.getInstance().getTime())
                        .createArchetypeDTO();
        archetypeDTO.setLastUpdate(Calendar.getInstance().getTime());
        GenericCMElementDAO<ArchetypeDTO> dao = CMElementDAOFactory.getInstance().getDAO(ArchetypeDTO.class);
        dao.insert(archetypeDTO);
        Collection<ArchetypeDTO> archetypeDTOs = dao.searchAll();
        assertEquals(1, archetypeDTOs.size());

        archetypeDTO = archetypeDTOs.iterator().next();
        assertEquals("testSrc", archetypeDTO.getSource());

        archetypeDTO.setSource("testSrc2");
        dao.update(archetypeDTO);
        archetypeDTOs = dao.searchByIds(Collections.singleton("testArchetypeId"));
        assertEquals(1, archetypeDTOs.size());
        archetypeDTO = archetypeDTOs.iterator().next();
        assertEquals("testSrc2", archetypeDTO.getSource());

        dao.remove("testArchetypeId");
        archetypeDTOs = dao.searchAll();
        assertEquals(0, archetypeDTOs.size());
    }
}
