package se.cambio.cds.gdl.converters.drools;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;

import junit.framework.TestCase;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.parser.GDLParser;
import se.cambio.openehr.controller.session.data.Archetypes;


public class TestConvertToDrools extends TestCase {

	public void setUp() throws Exception {
		Archetypes.loadArchetypes();
		parser = new GDLParser();
		guide = null;
	}

	public void testConvertBSACalculationGuide() throws Exception {
		parse("BSA_Calculation.v2.gdl");
		converter = new GDLDroolsConverter(guide);
		String output = converter.convertToDrools();
		//System.out.println(output);		
		//System.out.println(serializer.toDADL(guide));		
		compile(output);
	}
	
	public void testConvertTemperalGuide() throws Exception {
		parse("temperal.gdl");
		converter = new GDLDroolsConverter(guide);
		String output = converter.convertToDrools();
		//System.out.println(output);	
		compile(output);
	}
	
	public void testTermBindingsExist() throws Exception {
		parse("CHADVAS_Score_ICD10_bindings.v1.gdl");
		assertNotNull(guide.getOntology().getTermBindings());
		assertEquals(1, guide.getOntology().getTermBindings().size());
	}
	
	public void testCompileTemperalGuide() throws Exception {
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
