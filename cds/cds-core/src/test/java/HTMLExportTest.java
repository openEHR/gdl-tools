import org.junit.Test;
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

import static org.junit.Assert.assertTrue;


public class HTMLExportTest {

    @Test
    public void testGuideHTMLExport() throws InternalErrorException {
        Guide guide = new Guide();
        guide.setId("testGuideHTML");
        guide.setConcept("test");
        guide.getDescription().getDetails().put("en", new ResourceDescriptionItem());
        TermDefinition td = new TermDefinition();
        td.getTerms().put("test", new Term());
        guide.getOntology().getTermDefinitions().put("en", td);
        GuideHTMLExporter htmlExporter = new GuideHTMLExporter(guide, "en");
        String html = htmlExporter.convertToHTML();
        assertTrue(html.contains("testGuideHTML"));
    }

    @Test
    public void testStudyHTMLExport() throws InternalErrorException {
        Study study = new Study();
        study.setStudyId("testStudyHTML");
        study.getResourceDescription().getDetails().put("en", new ResourceDescriptionItem());
        study.getStudyDefinitions().put("en", new StudyDefinition());
        StudyHTMLExporter htmlExporter = new StudyHTMLExporter(study, "en");
        String html = htmlExporter.convertToHTML();
        assertTrue(html.contains("testStudyHTML"));
    }

    @Test
    public void testArchetypeHTMLExport() throws InternalErrorException, URISyntaxException {
        UserConfigurationManager.setParameter(UserConfigurationManager.ARCHETYPES_FOLDER_KW, HTMLExportTest.class.getClassLoader().getResource("archetypes").toURI().getPath());
        Archetypes.loadArchetypes();
        Archetype archetype = Archetypes.getArchetypeAOM("openEHR-EHR-OBSERVATION.body_weight.v1");
        ArchetypeHTMLExporter htmlExporter = new ArchetypeHTMLExporter(archetype, null, "en");
        htmlExporter.setIconPath("cds/cds-gui-swing/src/main/resources/img");
        String html = htmlExporter.convertToHTML();
        assertTrue(html.contains("openEHR-EHR-OBSERVATION.body_weight.v1"));
    }

    @Test
    public void testTemplateHTMLExport() throws InternalErrorException, URISyntaxException {
        UserConfigurationManager.setParameter(UserConfigurationManager.ARCHETYPES_FOLDER_KW, HTMLExportTest.class.getClassLoader().getResource("archetypes").toURI().getPath());
        UserConfigurationManager.setParameter(UserConfigurationManager.TEMPLATES_FOLDER_KW, HTMLExportTest.class.getClassLoader().getResource("templates").toURI().getPath());
        Archetypes.loadArchetypes();
        Templates.loadTemplates();
        Archetype archetype = Templates.getTemplateAOM("diagnosis_icd10");
        TemplateHTMExporter htmlExporter = new TemplateHTMExporter(archetype, "diagnosis_icd10", "en");
        htmlExporter.setIconPath("cds/cds-gui-swing/src/main/resources/img");
        String html = htmlExporter.convertToHTML();
        assertTrue(html.contains("diagnosis_icd10"));

    }

    @Test
    public void testTerminologyTMLExport() throws InternalErrorException, URISyntaxException {
        UserConfigurationManager.setParameter(UserConfigurationManager.TERMINOLOGIES_FOLDER_KW, HTMLExportTest.class.getClassLoader().getResource("terminologies").toURI().getPath());
        Terminologies.loadTerminologies();
        TerminologyHTMLExporter htmlExporter = new TerminologyHTMLExporter("ALERTS","en");
        String html = htmlExporter.convertToHTML();
        assertTrue(html.contains("Non compliant stroke prevention found, documented deviation older than 6 months"));
    }

    @Test
    public void testDSVHTMLExport() throws IOException, InternalErrorException {
        DecisionSupportView dsv = new DecisionSupportView();
        dsv.setDsViewId("testDSVHTML");
        dsv.getResourceDescription().getDetails().put("en", new ResourceDescriptionItem());
        dsv.getDecisionSupportViewDefinitions().put("en", new DecisionSupportViewDefinition());
        DSViewHTMLExporter htmlExporter = new DSViewHTMLExporter(dsv, "en");
        String html = htmlExporter.convertToHTML();
        assertTrue(html.contains("testDSVHTML"));
    }

    @Test
    public void testCDSAppExport() throws IOException, InternalErrorException {
        CDSApp cdsApp = new CDSApp();
        cdsApp.setCdsAppId("testCDSAppId");
        cdsApp.getResourceDescription().getDetails().put("en", new ResourceDescriptionItem());
        cdsApp.getCdsAppDefinitions().put("en", new CDSAppDefinition());
        CDSAppHTMLExporter htmlExporter = new CDSAppHTMLExporter(cdsApp, "en");
        String html = htmlExporter.convertToHTML();
        assertTrue(html.contains("testCDSAppId"));
    }

    @Test
    public void testScenarioExport() throws IOException, InternalErrorException {
        Scenario scenario = new Scenario();
        scenario.setScenarioId("testScenarioId");
        scenario.getResourceDescription().getDetails().put("en", new ResourceDescriptionItem());
        scenario.getScenarioDefinitions().put("en", new ScenarioDefinition());
        ScenarioHTMLExporter htmlExporter = new ScenarioHTMLExporter(scenario, "en");
        String html = htmlExporter.convertToHTML();
        assertTrue(html.contains("testScenarioId"));
    }
}