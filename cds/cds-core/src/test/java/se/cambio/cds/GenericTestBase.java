package se.cambio.cds;

import org.junit.Before;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import se.cambio.cds.configuration.CdsCoreConfiguration;
import se.cambio.cds.controller.cds.CdsDataManager;
import se.cambio.openehr.util.BeanProvider;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.IOException;
import java.net.URISyntaxException;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = CdsCoreConfiguration.class)
public abstract class GenericTestBase {

    @Autowired
    CdsDataManager cdsDataManager;

    @Autowired
    UserConfigurationManager userConfigurationManager;

    @Value("classpath:/archetypes")
    private Resource archetypesResource;

    @Value("classpath:/templates")
    private Resource templatesResource;

    @Value("classpath:/terminologies")
    private Resource terminologiesResource;

    @Value("classpath:/guidelines")
    private Resource guidelinesResource;

    @Before
    public void loadCM() throws InternalErrorException, URISyntaxException, IOException {
        userConfigurationManager.setArchetypesFolderPath(archetypesResource.getFile().getPath());
        userConfigurationManager.setTerminologiesFolderPath(terminologiesResource.getFile().getPath());
        userConfigurationManager.setTemplatesFolderPath(templatesResource.getFile().getPath());
        userConfigurationManager.setGuidelinesFolderPath(guidelinesResource.getFile().getPath());
        BeanProvider.setActiveProfiles("cm-admin-plain-service", "terminology-plain-service", "cm-admin-file-dao", "rule-dummy-engine");
    }
}
