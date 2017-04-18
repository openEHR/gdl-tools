package se.cambio.cds.gdl.converters.drools;

import org.springframework.beans.factory.annotation.Autowired;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import se.cambio.cds.controller.session.data.Guides;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.openehr.controller.session.data.ArchetypeManager;


import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

public class DroolsGuideExportPluginTest extends GDLTestCase {

    @Autowired
    ArchetypeManager archetypeManager;

    @Autowired
    Guides guides;

    DroolsGuideExportPlugin droolsGuideExportPlugin;

    @BeforeClass
    public void init() {
        droolsGuideExportPlugin = new DroolsGuideExportPlugin(archetypeManager);
    }


    @Test
    public void testGetPluginName() {
        assertThat(droolsGuideExportPlugin.getPluginName(), equalTo("Drools"));
    }

    @Test
    public void testCompile() {
        Guide guide = guides.getGuide("Stroke_risks.v2");
        assertThat(droolsGuideExportPlugin.compile(guide), notNullValue());

    }

    @Test
    public void testGetSource() throws Exception {
        Guide guide = guides.getGuide("Stroke_risks.v2");
        String source = droolsGuideExportPlugin.getSource(guide);
        assertThat(source, notNullValue());
    }

}