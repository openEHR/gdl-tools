package se.cambio.cds.gdl.editor;

import junit.framework.TestCase;
import org.apache.log4j.Logger;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.parser.GDLParser;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.controller.terminology.session.data.Terminologies;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.UserConfigurationManager;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;


public class GDLEditorMainTest extends TestCase {

    protected static String MAIN_GUIDE_REPOSITORY_PATH = UserConfigurationManager.getGuidesFolder().getPath()+File.separator;

    private static boolean setUpIsDone = false;

    public void setUp() {
        if (!setUpIsDone){
            //Load archetypes and templates
            try{
                UserConfigurationManager.setParameter(UserConfigurationManager.TERMINOLOGIES_FOLDER_KW, GDLEditorMainTest.class.getClassLoader().getResource("terminologies").toURI().getPath());
                Terminologies.loadTerminologies();
                UserConfigurationManager.setParameter(UserConfigurationManager.ARCHETYPES_FOLDER_KW, GDLEditorMainTest.class.getClassLoader().getResource("archetypes").toURI().getPath());
                Archetypes.loadArchetypes();
                UserConfigurationManager.setParameter(UserConfigurationManager.TEMPLATES_FOLDER_KW, GDLEditorMainTest.class.getClassLoader().getResource("templates").toURI().getPath());
                Templates.loadTemplates();
            }catch(Exception e){
                ExceptionHandler.handle(e);
                fail("Exception caught loading archetypes and templates");
            }
            setUpIsDone = true;
        }
    }


    public void testCMGuides()  throws Exception{
        UserConfigurationManager.setParameter(UserConfigurationManager.LANGUAGE,"en");
        File mainGuideDir = new File(GDLEditorMainTest.class.getClassLoader().getResource("guidelines").getPath());
        for (File file : mainGuideDir.listFiles()) {
            if (file.getName().endsWith(".gdl")){
                Logger.getLogger(GDLEditorMainTest.class).info("Testing guideline '"+file.getName()+"'");
                FileInputStream fis = new FileInputStream(file);
                InputStreamReader in = new InputStreamReader(fis, "UTF-8");
                String guideStr = IOUtils.toString(in).replaceAll("\\r\\n", "\n");
                Guide guide = new GDLParser().parse(new ByteArrayInputStream(guideStr.getBytes()));
                String guideStr2 = GuideUtil.serializeGuide(guide);
                assertEquals(guideStr, guideStr2);
            }
        }
    }
}
