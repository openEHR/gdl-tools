import junit.framework.TestCase;
import org.openehr.am.archetype.Archetype;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.ResourceDescriptionItem;
import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.model.study.Study;
import se.cambio.cds.model.study.StudyDefinition;
import se.cambio.cds.util.exporter.html.*;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.controller.terminology.session.data.Terminologies;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.net.URISyntaxException;

/**
 * User: iago.corbal
 * Date: 2014-09-12
 * Time: 15:59
 */
public class HTMLExportTest extends TestCase {

    public void testGuideHTMLExport(){
        Guide guide = new Guide();
        guide.setId("testGuideHTML");
        guide.setConcept("test");
        guide.getDescription().getDetails().put("en", new ResourceDescriptionItem());
        TermDefinition td = new TermDefinition();
        td.getTerms().put("test", new Term());
        guide.getOntology().getTermDefinitions().put("en", td);
        GuideHTMLExporter guideHTMLExporter = new GuideHTMLExporter(guide, "en");
        try {
            String html = guideHTMLExporter.convertToHTML();
            assertTrue(html.contains("testGuideHTML"));
        } catch (InternalErrorException e) {
            e.printStackTrace();
            fail();
        }
    }

    public void testStudyHTMLExport(){
        Study study = new Study();
        study.setStudyId("testStudyHTML");
        study.getResourceDescription().getDetails().put("en", new ResourceDescriptionItem());
        study.getStudyDefinitions().put("en", new StudyDefinition());
        StudyHTMLExporter studyHTMLExporter = new StudyHTMLExporter(study, "en");
        try {
            String html = studyHTMLExporter.convertToHTML();
            assertTrue(html.contains("testStudyHTML"));
        } catch (InternalErrorException e) {
            e.printStackTrace();
            fail();
        }
    }

    public void testArchetypeHTMLExport(){
        try {
            UserConfigurationManager.setParameter(UserConfigurationManager.ARCHETYPES_FOLDER_KW, ArchetypeHTMLExporter.class.getClassLoader().getResource("archetypes").toURI().getPath());
        } catch (URISyntaxException e) {
            fail();
        }
        try {
            Archetypes.loadArchetypes();
        } catch (InternalErrorException e) {
            e.printStackTrace();
            fail();
        }
        Archetype archetype = Archetypes.getArchetypeAOM("openEHR-EHR-OBSERVATION.body_weight.v1");
        ArchetypeHTMLExporter archetypeHTMLExporter = new ArchetypeHTMLExporter(archetype, null, "en");
        archetypeHTMLExporter.setIconPath("cds/cds-gui-swing/src/main/resources/img");
        try {
            String html = archetypeHTMLExporter.convertToHTML();
            assertTrue(html.contains("openEHR-EHR-OBSERVATION.body_weight.v1"));
        } catch (InternalErrorException e) {
            e.printStackTrace();
            fail();
        }
    }

    public void testTemplateHTMLExport(){
        try {
            UserConfigurationManager.setParameter(UserConfigurationManager.ARCHETYPES_FOLDER_KW, ArchetypeHTMLExporter.class.getClassLoader().getResource("archetypes").toURI().getPath());
            UserConfigurationManager.setParameter(UserConfigurationManager.TEMPLATES_FOLDER_KW, ArchetypeHTMLExporter.class.getClassLoader().getResource("templates").toURI().getPath());
        } catch (URISyntaxException e) {
            fail();
        }
        try {
            Archetypes.loadArchetypes();
            Templates.loadTemplates();
        } catch (InternalErrorException e) {
            e.printStackTrace();
            fail();
        }
        Archetype archetype = Templates.getTemplateAOM("diagnosis_icd10");
        TemplateHTMExporter templateHTMExporter = new TemplateHTMExporter(archetype, "diagnosis_icd10", "en");
        templateHTMExporter.setIconPath("cds/cds-gui-swing/src/main/resources/img");
        try {
            String html = templateHTMExporter.convertToHTML();
            assertTrue(html.contains("diagnosis_icd10"));
        } catch (InternalErrorException e) {
            e.printStackTrace();
            fail();
        }
    }

    public void testTerminologyTMLExport(){
        try {
            UserConfigurationManager.setParameter(UserConfigurationManager.TERMINOLOGIES_FOLDER_KW, ArchetypeHTMLExporter.class.getClassLoader().getResource("terminologies").toURI().getPath());
        } catch (URISyntaxException e) {
            fail();
        }
        try {
            Terminologies.loadTerminologies();
            TerminologyHTMLExporter terminologyHTMLExporter = new TerminologyHTMLExporter("ALERTS","en");
            String html = terminologyHTMLExporter.convertToHTML();
            assertTrue(html.contains("Non compliant stroke prevention found, documented deviation older than 6 months"));
        } catch (InternalErrorException e) {
            e.printStackTrace();
            fail();
        }
    }
}
