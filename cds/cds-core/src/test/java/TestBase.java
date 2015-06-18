import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import se.cambio.cm.model.configuration.CmPersistenceConfig;
import se.cambio.cm.model.facade.administration.delegate.CMAdministrationFacadeDelegate;
import se.cambio.cm.model.facade.terminology.delegate.TerminologyFacadeDelegate;
import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.IOException;
import java.net.URISyntaxException;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = CmPersistenceConfig.class)
@ActiveProfiles({"cm-admin-plain-service", "terminology-plain-service", "cm-admin-file-dao"})
public class TestBase {

    @Autowired
    private CMAdministrationFacadeDelegate cmAdministrationFacadeDelegate;
    @Autowired
    private TerminologyFacadeDelegate terminologyFacadeDelegate;

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
        UserConfigurationManager.setCmFolder(UserConfigurationManager.ARCHETYPES_FOLDER_KW, archetypesResource.getFile().getPath());
        UserConfigurationManager.setCmFolder(UserConfigurationManager.TERMINOLOGIES_FOLDER_KW, terminologiesResource.getFile().getPath());
        UserConfigurationManager.setCmFolder(UserConfigurationManager.TEMPLATES_FOLDER_KW, templatesResource.getFile().getPath());
        UserConfigurationManager.setCmFolder(UserConfigurationManager.GUIDES_FOLDER_KW, guidelinesResource.getFile().getPath());
        OpenEHRSessionManager.setCmAdministrationFacadeDelegate(cmAdministrationFacadeDelegate);
        OpenEHRSessionManager.setTerminologyFacadeDelegate(terminologyFacadeDelegate);
    }

    @Test
    public void test() {

    }
}
