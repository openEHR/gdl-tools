package se.cambio.cm.configuration;


import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.testng.AbstractTestNGSpringContextTests;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import se.cambio.cm.model.generic.dao.GenericCMElementDAO;
import se.cambio.openehr.util.UserConfigurationManager;

import java.io.IOException;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.IsNull.notNullValue;

@ContextConfiguration(classes = CmPersistenceConfig.class)
@ActiveProfiles({"cm-admin-file-dao"})
public class FileClinicalModelsPersistenceConfigurationTest extends AbstractTestNGSpringContextTests {

    @Autowired
    UserConfigurationManager userConfigurationManager;

    @Value("classpath:/archetypes")
    Resource archetypeResource;

    @BeforeClass
    public void setUp() throws IOException {
        userConfigurationManager.setArchetypesFolderPath(archetypeResource.getURL().getPath());
    }

    @Autowired
    ApplicationContext applicationContext;

    @Test
    public void should_use_file_archetype_dao() {
        GenericCMElementDAO archetypeDAO = applicationContext.getBean("ArchetypeDAO", GenericCMElementDAO.class);
        assertThat(archetypeDAO, notNullValue());
    }
}