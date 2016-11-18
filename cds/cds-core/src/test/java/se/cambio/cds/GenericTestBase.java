package se.cambio.cds;

import org.junit.Before;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import se.cambio.cds.controller.cds.CDSManager;
import se.cambio.openehr.util.BeanProvider;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.configuration.CdsConfiguration;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.IOException;
import java.net.URISyntaxException;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = CdsConfiguration.class)
public class GenericTestBase {

    @Autowired
    CDSManager cdsManager;

    @Value("classpath:/archetypes")
    Resource archetypesResource;

    @Value("classpath:/templates")
    Resource templatesResource;

    @Value("classpath:/terminologies")
    Resource terminologiesResource;

    @Value("classpath:/guidelines")
    Resource guidelinesResource;

    @Before
    public void loadCM() throws InternalErrorException, URISyntaxException, IOException {
        UserConfigurationManager.instance().setArchetypesFolderPath(archetypesResource.getFile().getPath());
        UserConfigurationManager.instance().setTerminologiesFolderPath(terminologiesResource.getFile().getPath());
        UserConfigurationManager.instance().setTemplatesFolderPath(templatesResource.getFile().getPath());
        UserConfigurationManager.instance().setGuidelinesFolderPath(guidelinesResource.getFile().getPath());
        BeanProvider.setActiveProfiles("cm-admin-plain-service", "terminology-plain-service", "cm-admin-file-dao", "rule-dummy-engine");
    }
}
