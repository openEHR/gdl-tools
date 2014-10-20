package se.cambio.cds.gdl.editor;

import junit.framework.TestCase;
import org.apache.log4j.Logger;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.parser.GDLParser;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.controller.session.data.Terminologies;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.UserConfigurationManager;

import java.io.*;


public class GDLEditorMainTest extends TestCase {

    protected static String MAIN_GUIDE_REPOSITORY_PATH = UserConfigurationManager.getGuidesFolder().getPath()+File.separator;

    private static boolean setUpIsDone = false;

    public void setUp() {
        if (!setUpIsDone){
            //Load archetypes and templates
            try{
                UserConfigurationManager.setParameter(UserConfigurationManager.TERMINOLOGIES_FOLDER_KW, GDLEditorMainTest.class.getClassLoader().getResource("terminologies").toURI().getPath());
                UserConfigurationManager.setParameter(UserConfigurationManager.ARCHETYPES_FOLDER_KW, GDLEditorMainTest.class.getClassLoader().getResource("archetypes").toURI().getPath());
                UserConfigurationManager.setParameter(UserConfigurationManager.TEMPLATES_FOLDER_KW, GDLEditorMainTest.class.getClassLoader().getResource("templates").toURI().getPath());
            }catch(Exception e){
                ExceptionHandler.handle(e);
                fail("Exception caught loading archetypes and templates");
            }
            setUpIsDone = true;
        }
    }

    public void testCompareSerializedGuides() throws Exception {
        UserConfigurationManager.setParameter(UserConfigurationManager.LANGUAGE,"en");
        File mainGuideDir = new File(GDLEditorMainTest.class.getClassLoader().getResource("guidelines").getPath());
        for (File file : mainGuideDir.listFiles()) {
            if (file.getName().endsWith(".gdl")){
                Logger.getLogger(GDLEditorMainTest.class).info("Testing guideline '"+file.getName()+"'");
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
        Guide guide = new GDLParser().parse(new ByteArrayInputStream(guideStr.getBytes()));
        return GuideUtil.serializeGuide(guide);
    }
}
