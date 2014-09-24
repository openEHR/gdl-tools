import junit.framework.TestCase;
import org.openehr.am.archetype.Archetype;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.ResourceDescriptionItem;
import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.model.app.CDSApp;
import se.cambio.cds.model.app.CDSAppDefinition;
import se.cambio.cds.model.scenario.Scenario;
import se.cambio.cds.model.scenario.ScenarioDefinition;
import se.cambio.cds.model.study.Study;
import se.cambio.cds.model.study.StudyDefinition;
import se.cambio.cds.model.view.DecisionSupportView;
import se.cambio.cds.model.view.DecisionSupportViewDefinition;
import se.cambio.cds.util.export.html.*;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.controller.terminology.session.data.Terminologies;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.IOException;
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
        GuideHTMLExporter htmlExporter = new GuideHTMLExporter(guide, "en");
        try {
            String html = htmlExporter.convertToHTML();
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
        StudyHTMLExporter htmlExporter = new StudyHTMLExporter(study, "en");
        try {
            String html = htmlExporter.convertToHTML();
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
        ArchetypeHTMLExporter htmlExporter = new ArchetypeHTMLExporter(archetype, null, "en");
        htmlExporter.setIconPath("cds/cds-gui-swing/src/main/resources/img");
        try {
            String html = htmlExporter.convertToHTML();
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
        TemplateHTMExporter htmlExporter = new TemplateHTMExporter(archetype, "diagnosis_icd10", "en");
        htmlExporter.setIconPath("cds/cds-gui-swing/src/main/resources/img");
        try {
            String html = htmlExporter.convertToHTML();
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
            TerminologyHTMLExporter htmlExporter = new TerminologyHTMLExporter("ALERTS","en");
            String html = htmlExporter.convertToHTML();
            assertTrue(html.contains("Non compliant stroke prevention found, documented deviation older than 6 months"));
        } catch (InternalErrorException e) {
            e.printStackTrace();
            fail();
        }
    }

    public void testDSVHTMLExport() throws IOException {
        DecisionSupportView dsv = new DecisionSupportView();
        dsv.setDsViewId("testDSVHTML");
        dsv.getResourceDescription().getDetails().put("en", new ResourceDescriptionItem());
        dsv.getDecisionSupportViewDefinitions().put("en", new DecisionSupportViewDefinition());
        DSViewHTMLExporter htmlExporter = new DSViewHTMLExporter(dsv, "en");
        try {
            String html = htmlExporter.convertToHTML();
            assertTrue(html.contains("testDSVHTML"));
        } catch (InternalErrorException e) {
            e.printStackTrace();
            fail();
        }
    }

    public void testCDSAppExport() throws IOException {
        CDSApp cdsApp = new CDSApp();
        cdsApp.setCdsAppId("testCDSAppId");
        cdsApp.getResourceDescription().getDetails().put("en", new ResourceDescriptionItem());
        cdsApp.getCdsAppDefinitions().put("en", new CDSAppDefinition());
        CDSAppHTMLExporter htmlExporter = new CDSAppHTMLExporter(cdsApp, "en");
        try {
            String html = htmlExporter.convertToHTML();
            assertTrue(html.contains("testCDSAppId"));
            //Files.write(Paths.get("testCDSApp.html"), html.getBytes());
        } catch (InternalErrorException e) {
            e.printStackTrace();
            fail();
        }
    }

    public void testScenarioExport() throws IOException {
        Scenario scenario = new Scenario();
        scenario.setScenarioId("testScenarioId");
        scenario.getResourceDescription().getDetails().put("en", new ResourceDescriptionItem());
        scenario.getScenarioDefinitions().put("en", new ScenarioDefinition());
        ScenarioHTMLExporter htmlExporter = new ScenarioHTMLExporter(scenario, "en");
        try {
            String html = htmlExporter.convertToHTML();
            assertTrue(html.contains("testScenarioId"));
            //Files.write(Paths.get("testScenario.html"), html.getBytes());
        } catch (InternalErrorException e) {
            e.printStackTrace();
            fail();
        }
    }
}
