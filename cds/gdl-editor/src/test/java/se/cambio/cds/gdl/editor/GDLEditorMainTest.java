package se.cambio.cds.gdl.editor;
import junit.framework.TestCase;
import org.apache.log4j.Logger;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.gdl.converters.drools.CompilationErrorException;
import se.cambio.cds.gdl.converters.drools.CompilationManager;
import se.cambio.cds.gdl.converters.drools.GDLDroolsConverter;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.parser.GDLParser;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;


public class GDLEditorMainTest extends TestCase {

    protected static String MAIN_GUIDE_REPOSITORY_PATH = UserConfigurationManager.getGuidesFolder().getPath()+File.separator;

    private static boolean setUpIsDone = false;

    public void setUp() {
        if (!setUpIsDone){
            //Load archetypes and templates
            try{
                Archetypes.loadArchetypes();
                Templates.loadTemplates();
            }catch(Exception e){
                fail("Exception caught loading archetypes and templates: \n"+e);
            }
            setUpIsDone = true;
        }
    }


    public void testCMGuides()  throws Exception{
        File mainGuideDir = new File(MAIN_GUIDE_REPOSITORY_PATH);
        if(mainGuideDir.exists() && mainGuideDir.isDirectory()){
            for (File file : mainGuideDir.listFiles()) {
                if (file.getName().endsWith(".gdl")){
                    Logger.getLogger(GDLEditorMainTest.class).info("Testing guideline '"+file.getName()+"'");
                    FileInputStream fis = new FileInputStream(file);
                    InputStreamReader in = new InputStreamReader(fis, "UTF-8");
                    String guideStr = IOUtils.toString(in).replaceAll("\\r\\n", "\n");
                    Guide guide = new GDLParser().parse(new ByteArrayInputStream(guideStr.getBytes()));
                    GDLEditor editor = new GDLEditor(guide);
                    String guideStr2 = editor.serializeCurrentGuide();
                    assertEquals(guideStr, guideStr2);
                }
            }
        }
    }

    public static Collection<GuideDTO> getGuideDTOs(Collection<String> paths) throws Exception{
        Collection<GuideDTO> guideDTOs = new ArrayList<GuideDTO>();
        for (String path : paths) {
            guideDTOs.add(getGuideDTO(path));
        }
        return guideDTOs;
    }

    public static GuideDTO getGuideDTO(String path) throws Exception{
        File file = new File(path);
        FileInputStream fis = new FileInputStream(file);
        InputStreamReader in = new InputStreamReader(fis, "UTF-8");
        String guideStr = IOUtils.toString(in);
        Guide guide = GuideUtil.parseGuide(new ByteArrayInputStream(guideStr.getBytes()));
        byte[] compiledGuide = null;//compile(guide);
        return new GuideDTO(
                guide.getId(),
                guideStr,
                IOUtils.getBytes(guide),
                compiledGuide,
                true,
                Calendar.getInstance().getTime());
    }

    public static String createDroolsGuide(Guide guide) throws InternalErrorException {
        return new GDLDroolsConverter(guide).convertToDrools();
    }

    public static byte[] compile(Guide guide) throws InternalErrorException{
        try {
            return CompilationManager.compile(createDroolsGuide(guide));
        } catch (CompilationErrorException e) {
            throw new InternalErrorException(e);
        }
    }
}
