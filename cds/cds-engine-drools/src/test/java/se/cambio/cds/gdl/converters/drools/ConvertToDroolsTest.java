package se.cambio.cds.gdl.converters.drools;

import org.junit.Before;
import org.junit.Test;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.parser.GDLParser;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.UserConfigurationManager;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;


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
		//System.out.println(output);		
		//System.out.println(serializer.toDADL(guide));		
		compile(output);
	}

    @Test
	public void shouldConvertTemperalGuide() throws Exception {
		parse("temperal.gdl");
		converter = new GDLDroolsConverter(guide, ArchetypeManager.getInstance());
		String output = converter.convertToDrools();
		//System.out.println(output);	
		compile(output);
	}

    @Test
    public void shouldTermBindingsExist() throws Exception {
		parse("CHADVAS_Score_ICD10_bindings.v1.gdl");
		assertNotNull(guide.getOntology().getTermBindings());
		assertEquals(1, guide.getOntology().getTermBindings().size());
	}

    @Test
    public void shouldCompileTemperalGuide() throws Exception {
		String guide = readFile("temperal.drools");
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
		while(line != null) {
			buf.append(line);
			buf.append("\r\n");
			line = reader.readLine();
		}
		return buf.toString();
	}

	public byte[] compile(String guide) {
		try {
			return CompilationManager.compile(guide);
		} catch (CompilationErrorException e) {
			e.printStackTrace();
			fail("failed to compile guide..");
			return null;
		}
	}

	private GDLParser parser;
	private Guide guide;
	private GDLDroolsConverter converter;
}
