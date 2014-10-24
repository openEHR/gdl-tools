import org.junit.Before;
import org.junit.Test;
import org.openehr.am.archetype.Archetype;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.ResourceDescriptionItem;
import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.model.app.CDSApp;
import se.cambio.cds.model.app.CDSAppDefinition;
import se.cambio.cds.model.kb.instance.KBInstance;
import se.cambio.cds.model.kb.instance.KBInstanceDefinition;
import se.cambio.cds.model.orderset.OrderSet;
import se.cambio.cds.model.orderset.OrderSetDefinition;
import se.cambio.cds.model.scenario.Scenario;
import se.cambio.cds.model.scenario.ScenarioDefinition;
import se.cambio.cds.model.study.Study;
import se.cambio.cds.model.study.StudyDefinition;
import se.cambio.cds.model.view.DecisionSupportView;
import se.cambio.cds.model.view.DecisionSupportViewDefinition;
import se.cambio.cds.util.export.CMElementUtil;
import se.cambio.cds.util.export.html.*;
import se.cambio.cm.model.kb.instance.dto.KBInstanceDTO;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.IOException;
import java.net.URISyntaxException;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertThat;


public class HTMLExportTest {

    @Before
    public void loadCM() throws URISyntaxException, InternalErrorException {
        CMUtil.testLoadCM();
    }

    @Test
    public void shouldExportGuideToHTML() throws InternalErrorException {
        Guide guide = new Guide();
        guide.setId("testGuideHTML");
        guide.setConcept("test");
        guide.getDescription().getDetails().put("en", new ResourceDescriptionItem());
        TermDefinition td = new TermDefinition();
        td.getTerms().put("test", new Term());
        guide.getOntology().getTermDefinitions().put("en", td);
        GuideHTMLExporter htmlExporter = new GuideHTMLExporter(ArchetypeManager.getInstance());
        String html = htmlExporter.convertToHTML(guide, "en");
        assertThat(html, containsString("testGuideHTML"));
    }

    @Test
    public void shouldExportStudyToHTML() throws InternalErrorException {
        Study study = new Study("testStudyHTML");
        study.getResourceDescription().getDetails().put("en", new ResourceDescriptionItem());
        study.getStudyDefinitions().put("en", new StudyDefinition());
        StudyHTMLExporter htmlExporter = new StudyHTMLExporter(ArchetypeManager.getInstance());
        String html = htmlExporter.convertToHTML(study, "en");
        assertThat(html, containsString("testStudyHTML"));
    }

    @Test
    public void shouldExportArchetypeToHTML() throws InternalErrorException, InstanceNotFoundException {
        Archetype archetype = ArchetypeManager.getInstance().getArchetypes().getArchetypeAOMById("openEHR-EHR-OBSERVATION.body_weight.v1");
        ArchetypeHTMLExporter htmlExporter = new ArchetypeHTMLExporter(ArchetypeManager.getInstance());
        htmlExporter.setIconPath("cds/cds-gui-swing/src/main/resources/img");
        String html = htmlExporter.convertToHTML(archetype, "en");
        assertThat(html, containsString("openEHR-EHR-OBSERVATION.body_weight.v1"));
    }

    @Test
    public void shouldExportTemplateToHTML() throws InternalErrorException, InstanceNotFoundException {
        Archetype archetype = ArchetypeManager.getInstance().getTemplates().getTemplateAOMById("diagnosis_icd10");
        TemplateHTMLExporter htmlExporter = new TemplateHTMLExporter(ArchetypeManager.getInstance());
        htmlExporter.setIconPath("cds/cds-gui-swing/src/main/resources/img");
        htmlExporter.setTemplateId("diagnosis_icd10");
        String html = htmlExporter.convertToHTML(archetype, "en");
        assertThat(html, containsString("diagnosis_icd10"));
    }

    @Test
    public void shouldExportTerminologyToHTML() throws InternalErrorException, InstanceNotFoundException {
        TerminologyHTMLExporter htmlExporter = new TerminologyHTMLExporter(ArchetypeManager.getInstance());
        String html = htmlExporter.convertToHTML("ALERTS", "en");
        assertThat(html, containsString("Non compliant stroke prevention found, documented deviation older than 6 months"));
    }

    @Test
    public void shouldExportViewToHTML() throws IOException, InternalErrorException {
        DecisionSupportView decisionSupportView = new DecisionSupportView();
        decisionSupportView.setDsViewId("testDSVHTML");
        decisionSupportView.getResourceDescription().getDetails().put("en", new ResourceDescriptionItem());
        decisionSupportView.getDecisionSupportViewDefinitions().put("en", new DecisionSupportViewDefinition());
        DSViewHTMLExporter htmlExporter = new DSViewHTMLExporter(ArchetypeManager.getInstance());
        String html = htmlExporter.convertToHTML(decisionSupportView, "en");
        assertThat(html, containsString("testDSVHTML"));
    }

    @Test
    public void shouldExportAppToHTML() throws IOException, InternalErrorException {
        CDSApp cdsApp = new CDSApp();
        cdsApp.setCdsAppId("testCDSAppId");
        cdsApp.getResourceDescription().getDetails().put("en", new ResourceDescriptionItem());
        cdsApp.getCdsAppDefinitions().put("en", new CDSAppDefinition());
        CDSAppHTMLExporter htmlExporter = new CDSAppHTMLExporter(ArchetypeManager.getInstance());
        String html = htmlExporter.convertToHTML(cdsApp, "en");
        assertThat(html, containsString("testCDSAppId"));
    }

    @Test
    public void shouldExportScenarioToHTML() throws IOException, InternalErrorException {
        Scenario scenario = new Scenario("testScenarioId");
        scenario.getResourceDescription().getDetails().put("en", new ResourceDescriptionItem());
        scenario.getScenarioDefinitions().put("en", new ScenarioDefinition());
        ScenarioHTMLExporter htmlExporter = new ScenarioHTMLExporter(ArchetypeManager.getInstance());
        String html = htmlExporter.convertToHTML(scenario, "en");
        assertThat(html, containsString("testScenarioId"));
    }

    @Test
    public void shouldExportInstanceToHTML() throws IOException, InternalErrorException {
        KBInstance kbInstance = new KBInstance("testKBInstanceId");
        kbInstance.getResourceDescription().getDetails().put("en", new ResourceDescriptionItem());
        kbInstance.getKbInstanceDefinitions().put("en", new KBInstanceDefinition());
        KBInstanceHTMLExporter htmlExporter = new KBInstanceHTMLExporter(ArchetypeManager.getInstance());
        KBInstanceDTO kbInstanceDTO = new KBInstanceDTO();
        CMElementUtil.fill(kbInstanceDTO, kbInstance);
        String html = htmlExporter.convertToHTML(kbInstance, "en");
        assertThat(html, containsString("testKBInstanceId"));
    }

    @Test
    public void shouldExportOrderSetToHTML() throws IOException, InternalErrorException {
        OrderSet orderSet = new OrderSet("testOrderSetId");
        orderSet.getResourceDescription().getDetails().put("en", new ResourceDescriptionItem());
        orderSet.getOrderSetDefinitions().put("en", new OrderSetDefinition());
        OrderSetHTMLExporter htmlExporter = new OrderSetHTMLExporter(ArchetypeManager.getInstance());
        String html = htmlExporter.convertToHTML(orderSet, "en");
        assertThat(html, containsString("testOrderSetId"));
    }
}
