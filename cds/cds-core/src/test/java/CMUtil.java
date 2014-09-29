import junit.framework.TestCase;
import se.cambio.cds.util.export.html.ArchetypeHTMLExporter;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.controller.terminology.session.data.Terminologies;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.net.URISyntaxException;

/**
 * User: Iago.Corbal
 * Date: 2014-09-29
 * Time: 09:30
 */
public class CMUtil extends TestCase{
    public static void testLoadCM(){
        try {
            UserConfigurationManager.setParameter(UserConfigurationManager.TERMINOLOGIES_FOLDER_KW, ArchetypeHTMLExporter.class.getClassLoader().getResource("terminologies").toURI().getPath());
            UserConfigurationManager.setParameter(UserConfigurationManager.ARCHETYPES_FOLDER_KW, ArchetypeHTMLExporter.class.getClassLoader().getResource("archetypes").toURI().getPath());
            UserConfigurationManager.setParameter(UserConfigurationManager.TEMPLATES_FOLDER_KW, ArchetypeHTMLExporter.class.getClassLoader().getResource("templates").toURI().getPath());
        } catch (URISyntaxException e) {
            fail();
        }
        try {
            Terminologies.loadTerminologies();
            Archetypes.loadArchetypes();
            Templates.loadTemplates();
        } catch (InternalErrorException e) {
            e.printStackTrace();
            fail();
        }
    }
}
