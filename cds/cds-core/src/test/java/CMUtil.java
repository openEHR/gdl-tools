import org.junit.Test;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.controller.terminology.session.data.Terminologies;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.net.URISyntaxException;

public class CMUtil {

    @Test
    public static void testLoadCM() throws URISyntaxException, InternalErrorException {
        UserConfigurationManager.setParameter(UserConfigurationManager.TERMINOLOGIES_FOLDER_KW, CMUtil.class.getClassLoader().getResource("terminologies").toURI().getPath());
        UserConfigurationManager.setParameter(UserConfigurationManager.ARCHETYPES_FOLDER_KW, CMUtil.class.getClassLoader().getResource("archetypes").toURI().getPath());
        UserConfigurationManager.setParameter(UserConfigurationManager.TEMPLATES_FOLDER_KW, CMUtil.class.getClassLoader().getResource("templates").toURI().getPath());
        Terminologies.loadTerminologies();
        Archetypes.loadArchetypes();
        Templates.loadTemplates();
    }
}
