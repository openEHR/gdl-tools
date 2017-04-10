package se.cambio.cds.gdl.editor;

import org.apache.commons.io.IOUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.parser.GDLParser;
import se.cambio.cm.model.facade.administration.delegate.ClinicalModelsService;
import se.cambio.openehr.util.BeanProvider;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.configuration.CdsConfiguration;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.*;
import java.net.URISyntaxException;

import static junit.framework.Assert.assertNotNull;
import static junit.framework.TestCase.assertEquals;


@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = CdsConfiguration.class)
@ActiveProfiles({"cm-admin-plain-service", "terminology-plain-service", "cm-admin-file-dao"})
public class GDLEditorMainTest {

    @Value("classpath:/archetypes")
    Resource archetypesResource;

    @Value("classpath:/templates")
    Resource templatesResource;

    @Value("classpath:/terminologies")
    Resource terminologiesResource;

    @Value("classpath:/guidelines")
    Resource guidelinesResource;

    @Autowired
    UserConfigurationManager userConfigurationManager;

    @Before
    public void loadCM() throws InternalErrorException, URISyntaxException, IOException {
        userConfigurationManager.setArchetypesFolderPath(archetypesResource.getFile().getPath());
        userConfigurationManager.setTerminologiesFolderPath(terminologiesResource.getFile().getPath());
        userConfigurationManager.setTemplatesFolderPath(templatesResource.getFile().getPath());
        userConfigurationManager.setGuidelinesFolderPath(guidelinesResource.getFile().getPath());
    }

    @Test
    public void should_get_facades_implementation_from_annotations() {
        ClinicalModelsService clinicalModelsService = BeanProvider.getBean(ClinicalModelsService.class);
        assertNotNull(clinicalModelsService);
    }

    @Test
    public void testCompareSerializedGuides() throws Exception {
        userConfigurationManager.setLanguage("en");
        File mainGuideDir = new File(GDLEditorMainTest.class.getClassLoader().getResource("guidelines").getPath());
        for (File file : mainGuideDir.listFiles()) {
            if (file.getName().endsWith(".gdl")) {
                LoggerFactory.getLogger(GDLEditorMainTest.class).info("Testing guideline '" + file.getName() + "'");
                String originalGuideStr = readGuideFile(file);
                String output = parseAndReserializeGuide(originalGuideStr);
                assertEquals(originalGuideStr, output);
            }
        }
    }

    private static String readGuideFile(File file) throws IOException {
        FileInputStream fis = new FileInputStream(file);
        InputStreamReader in = new InputStreamReader(fis, "UTF-8");
        return IOUtils.toString(in).replaceAll("\\r\\n", "\n");
    }

    private static String parseAndReserializeGuide(String guideStr) throws Exception {
        Guide guide = new GDLParser().parse(new ByteArrayInputStream(guideStr.getBytes("UTF-8")));
        return GuideUtil.serializeGuide(guide);
    }
}
