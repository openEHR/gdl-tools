import junit.framework.TestCase;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.ResourceDescriptionItem;
import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.model.study.Study;
import se.cambio.cds.model.study.StudyDefinition;
import se.cambio.cds.util.exporter.html.GuideHTMLExporter;
import se.cambio.cds.util.exporter.html.StudyHTMLExporter;
import se.cambio.openehr.util.exceptions.InternalErrorException;

/**
 * User: iago.corbal
 * Date: 2014-09-12
 * Time: 15:59
 */
public class HMLExportTest extends TestCase {

    public void testGuideHTMLExport(){
        Guide guide = new Guide();
        guide.setId("test");
        guide.setConcept("test");
        guide.getDescription().getDetails().put("en", new ResourceDescriptionItem());
        TermDefinition td = new TermDefinition();
        td.getTerms().put("test", new Term());
        guide.getOntology().getTermDefinitions().put("en", td);
        GuideHTMLExporter guideHTMLExporter = new GuideHTMLExporter(guide, "en");
        try {
            guideHTMLExporter.convertToHTML();
        } catch (InternalErrorException e) {
            e.printStackTrace();
            fail();
        }
    }

    public void testStudyHTMLExport(){
        Study study = new Study();
        study.setStudyId("test");
        study.getResourceDescription().getDetails().put("en", new ResourceDescriptionItem());
        study.getStudyDefinitions().put("en", new StudyDefinition());
        StudyHTMLExporter studyHTMLExporter = new StudyHTMLExporter(study, "en");
        try {
            studyHTMLExporter.convertToHTML();
        } catch (InternalErrorException e) {
            e.printStackTrace();
            fail();
        }
    }
}
