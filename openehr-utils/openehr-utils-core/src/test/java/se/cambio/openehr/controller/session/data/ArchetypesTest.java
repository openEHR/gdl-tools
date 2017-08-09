package se.cambio.openehr.controller.session.data;

import org.openehr.am.archetype.Archetype;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.testng.AbstractTestNGSpringContextTests;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.controller.session.configuration.ClinicalModelsCacheConfiguration;
import se.cambio.openehr.util.UserConfigurationManager;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.concurrent.Executors;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;


@ContextConfiguration(classes = {ClinicalModelsCacheConfiguration.class})
@ActiveProfiles({ "cm-admin-file-dao"})
public class ArchetypesTest extends AbstractTestNGSpringContextTests {

    @Value("classpath:/archetypes")
    private Resource archetypesResource;

    @Autowired
    ArchetypeManager archetypeManager;

    @Autowired
    UserConfigurationManager userConfigurationManager;

    Archetypes archetypes;

    @BeforeClass
    public void init() throws IOException {
        userConfigurationManager.setArchetypesFolderPath(archetypesResource.getFile().getPath());
        archetypes = new Archetypes(archetypeManager, Executors.newCachedThreadPool());
    }

    @Test
    public void testGetArchetypeAOMById() throws Exception {
        String archetypeId = "openEHR-EHR-OBSERVATION.body_weight.v1";
        Archetype archetype = archetypes.getArchetypeAOMById(archetypeId);
        assertThat(archetype, notNullValue());
    }

    @Test
    public void testProcessArchetype() {
        Collection<ArchetypeDTO> archetypeDTOs =
                archetypes.getCMElementByIds(Arrays.asList("openEHR-EHR-OBSERVATION.body_weight.v1", "openEHR-EHR-OBSERVATION.chadsvas_score.v1"));
        archetypes.processArchetypes(archetypeDTOs);
    }
}