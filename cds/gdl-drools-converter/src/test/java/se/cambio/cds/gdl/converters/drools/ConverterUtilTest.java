package se.cambio.cds.gdl.converters.drools;

import junit.framework.TestCase;

public class ConverterUtilTest extends TestCase {
	
	public void setUp() {
		converter = new GDLDroolsConverter(null);
	}
	
	public void testParseCodeWithDvText() {
		String code = converter.parseCode("new DvText(\"local::gt0100\")");
		assertEquals("gt0100", code);
	}
	
	public void testParseCodeWithDvTextPlusLabel() {
		String code = converter.parseCode("new DvText(\"local::gt0100|Hypertension|\")");
		assertEquals("gt0100", code);
	}
	
	public void testParseCodeWithDvTextAndQuotation() {
		String code = converter.parseCode("new DvText(\"'local::gt0100'\")");
		assertEquals("gt0100", code);
	}
	
	public void testParseCodeWithDvCodedText() {
		String code = converter.parseCode(" new DvCodedText(\"Dubois and Dubois\",\"local\",\"gt0008\")");
		assertEquals("gt0008", code);
	}
	
	private GDLDroolsConverter converter;	
}
