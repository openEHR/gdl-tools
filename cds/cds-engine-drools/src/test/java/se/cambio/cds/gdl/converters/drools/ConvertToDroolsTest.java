package se.cambio.cds.gdl.converters.drools;

import org.junit.Before;
import org.junit.Test;
import org.kie.api.KieServices;
import org.kie.api.builder.KieBuilder;
import org.kie.api.builder.KieFileSystem;
import org.kie.api.builder.Message;
import org.kie.api.io.Resource;
import org.kie.internal.io.ResourceFactory;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.parser.GDLParser;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.UserConfigurationManager;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;

import static org.junit.Assert.*;


public class ConvertToDroolsTest {

    @Before
    public void setUp() throws Exception {
        String archetypesFolderPath = ConvertToDroolsTest.class.getClassLoader().getResource("archetypes").getPath();
        UserConfigurationManager.setCmFolder(UserConfigurationManager.ARCHETYPES_FOLDER_KW, archetypesFolderPath);
        parser = new GDLParser();
        guide = null;
    }

    @Test
    public void testConvertBSACalculationGuide() throws Exception {
        parse("BSA_Calculation.v2.gdl");
        converter = new GDLDroolsConverter(guide, ArchetypeManager.getInstance());
        String output = converter.convertToDrools();
        compile(output);
    }

    @Test
    public void shouldConvertTemporalGuide() throws Exception {
        parse("temporal.gdl");
        converter = new GDLDroolsConverter(guide, ArchetypeManager.getInstance());
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
        String guide = readFile("temporal.drools");
        //System.out.println(guide);
        compile(guide);
    }

    private void parse(String input) throws Exception {
        guide = parser.parse(load(input));
        assertNotNull(guide);
    }

    private InputStream load(String name) throws Exception {
        return this.getClass().getClassLoader().getResourceAsStream(name);
    }

    private String readFile(String name) throws Exception {
        BufferedReader reader = new BufferedReader(new InputStreamReader(load(name)));
        StringBuffer buf = new StringBuffer();
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
