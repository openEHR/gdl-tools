import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import se.cambio.cm.configuration.CmServiceConfiguration;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.controller.ArchetypeObjectBundleManager;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Calendar;

import static org.junit.Assert.assertNotNull;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = CmServiceConfiguration.class)
public class ParsingTest {

    @Value("classpath:/archetypes")
    Resource archetypesResource;

    @Before
    public void initializeCM() throws URISyntaxException, IOException {
        UserConfigurationManager.setCmFolder(UserConfigurationManager.ARCHETYPES_FOLDER_KW, archetypesResource.getFile().getPath());
    }

    @Test
    public void testArchetypeParsing() throws InternalErrorException, InstanceNotFoundException, IOException {
        String archetypeId = "openEHR-EHR-OBSERVATION.body_weight.v1";
        File archetypeFile = new File(archetypesResource.getFile(), archetypeId + ".adl");
        FileInputStream fis = new FileInputStream(archetypeFile);
        String source = IOUtils.toString(fis);
        ArchetypeDTO archetypeDTO = new ArchetypeDTO(archetypeId, "adl", source, null, null, Calendar.getInstance().getTime());
        new ArchetypeObjectBundleManager(archetypeDTO, new ArchetypeManager()).buildArchetypeObjectBundleCustomVO();
        assertNotNull(archetypeDTO.getAom());
    }
}
