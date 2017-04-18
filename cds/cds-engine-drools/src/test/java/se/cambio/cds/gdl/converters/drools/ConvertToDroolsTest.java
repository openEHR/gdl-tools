package se.cambio.cds.gdl.converters.drools;

import org.kie.api.KieServices;
import org.kie.api.builder.KieBuilder;
import org.kie.api.builder.KieFileSystem;
import org.kie.api.builder.Message;
import org.kie.api.io.Resource;
import org.kie.internal.io.ResourceFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.parser.GDLParser;
import se.cambio.openehr.controller.session.data.ArchetypeManager;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;

import static org.junit.Assert.*;

public class ConvertToDroolsTest extends GDLTestCase{
    
    @Autowired
    ArchetypeManager archetypeManager;
    
	@BeforeClass
	public void setUp() throws Exception {
		String archetypesFolderPath = ConvertToDroolsTest.class.getClassLoader().getResource("archetypes").getPath();
        archetypeManager.getUserConfigurationManager().setArchetypesFolderPath(archetypesFolderPath);
		parser = new GDLParser();
		guide = null;
	}

    @Test
    public void testConvertBSACalculationGuide() throws Exception {
        parse("BSA_Calculation.v2.gdl");
        converter = new GDLDroolsConverter(guide, archetypeManager);
        String output = converter.convertToDrools();
        compile(output);
    }

    @Test
    public void shouldConvertTemporalGuide() throws Exception {
        parse("temporal.gdl");
        converter = new GDLDroolsConverter(guide, archetypeManager);
        String output = converter.convertToDrools();
        compile(output);
    }

    @Test
    public void shouldTermBindingsExist() throws Exception {
        parse("CHADVAS_Score_ICD10_bindings.v1.gdl");
        assertNotNull(guide.getOntology().getTermBindings());
        assertEquals(1, guide.getOntology().getTermBindings().size());
    }

    @Test
    public void shouldCompileTemporalGuide() throws Exception {
        String guide = readFile();
        compile(guide);
    }

    private void parse(String input) throws Exception {
        guide = parser.parse(load(input));
        assertNotNull(guide);
    }

    private InputStream load(String name) throws Exception {
        return this.getClass().getClassLoader().getResourceAsStream(name);
    }

    private String readFile() throws Exception {
        BufferedReader reader = new BufferedReader(new InputStreamReader(load("temporal.drools")));
        StringBuilder buf = new StringBuilder();
        String line = reader.readLine();
        while (line != null) {
            buf.append(line);
            buf.append("\r\n");
            line = reader.readLine();
        }
        return buf.toString();
    }

    private byte[] compile(String guide) {
        try {
            final KieServices kieServices = KieServices.Factory.get();
            final KieFileSystem kieFileSystem = kieServices.newKieFileSystem();
            Resource resource = ResourceFactory.newByteArrayResource(guide.getBytes("UTF8"));
            if (resource != null) {
                kieFileSystem.write("src/main/resources/test.drl", resource);
            }
            final KieBuilder kieBuilder = kieServices.newKieBuilder(kieFileSystem);
            kieBuilder.buildAll();
            if (kieBuilder.getResults().hasMessages(Message.Level.ERROR)) {
                fail("failed to compile guide..");
                throw new RuntimeException("Build Errors:\n" + kieBuilder.getResults().toString());
            }
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
            fail("failed to compile guide..");
        }
        return null;
    }

    private GDLParser parser;
    private Guide guide;
    private GDLDroolsConverter converter;
}
