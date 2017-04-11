import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.testng.AbstractTestNGSpringContextTests;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import se.cambio.cm.configuration.CmPersistenceConfig;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.archetype.dto.ArchetypeDTOBuilder;
import se.cambio.cm.model.generic.dao.GenericCMElementDAO;
import se.cambio.cm.model.util.CMElementDAOFactory;
import se.cambio.openehr.util.UserConfigurationManager;

import java.io.IOException;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;


@ContextConfiguration(classes = CmPersistenceConfig.class)
@ActiveProfiles({"cm-admin-file-dao"})
public class PersistenceTest extends AbstractTestNGSpringContextTests {

    @Autowired
    private CMElementDAOFactory cmElementDAOFactory;

    @Autowired
    UserConfigurationManager userConfigurationManager;

    @Value("classpath:/archetypes")
    Resource archetypeResource;

    @BeforeClass
    public void setUp() throws IOException {
        userConfigurationManager.setArchetypesFolderPath(archetypeResource.getURL().getPath());
    }

    @Test
    public void shouldAllowRoundTripForAllCMElements() {

        ArchetypeDTO archetypeDTO =
                new ArchetypeDTOBuilder()
                        .setId("testArchetypeId")
                        .setFormat("adl")
                        .setSource("testSrc")
                        .setLastUpdate(Calendar.getInstance().getTime())
                        .createArchetypeDTO();
        archetypeDTO.setLastUpdate(Calendar.getInstance().getTime());
        GenericCMElementDAO<ArchetypeDTO> dao = cmElementDAOFactory.getDAO(ArchetypeDTO.class);
        dao.insert(archetypeDTO);
        Collection<ArchetypeDTO> archetypeDTOs = dao.searchAll();
        assertThat(2, equalTo(archetypeDTOs.size()));

        archetypeDTOs = dao.searchByIds(Collections.singleton("testArchetypeId"));
        assertThat(1, equalTo(archetypeDTOs.size()));

        archetypeDTO = archetypeDTOs.iterator().next();
        assertThat("testSrc", equalTo(archetypeDTO.getSource()));

        archetypeDTO.setSource("testSrc2");
        dao.update(archetypeDTO);
        archetypeDTOs = dao.searchByIds(Collections.singleton("testArchetypeId"));
        assertThat(1, equalTo(archetypeDTOs.size()));
        archetypeDTO = archetypeDTOs.iterator().next();
        assertThat("testSrc2", equalTo(archetypeDTO.getSource()));

        dao.remove("testArchetypeId");
        archetypeDTOs = dao.searchAll();
        assertThat(1, equalTo(archetypeDTOs.size()));
    }
}
