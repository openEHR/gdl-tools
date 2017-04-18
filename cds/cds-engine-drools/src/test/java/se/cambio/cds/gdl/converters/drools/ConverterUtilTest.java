package se.cambio.cds.gdl.converters.drools;

import org.springframework.beans.factory.annotation.Autowired;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import se.cambio.openehr.controller.session.data.ArchetypeManager;

import static org.junit.Assert.assertEquals;

public class ConverterUtilTest extends GDLTestCase{

	@Autowired
	ArchetypeManager archetypeManager;

    @BeforeClass
	public void setUp() {
		converter = new GDLDroolsConverter(null, archetypeManager);
	}

    @Test
	public void shouldParseCodeWithDvText() {
		String code = converter.parseCode("new DvText(\"local::gt0100\")");
		assertEquals("gt0100", code);
	}

    @Test
	public void shouldParseCodeWithDvTextPlusLabel() {
		String code = converter.parseCode("new DvText(\"local::gt0100|Hypertension|\")");
		assertEquals("gt0100", code);
	}

    @Test
    public void shouldParseCodeWithDvTextAndQuotation() {
		String code = converter.parseCode("new DvText(\"'local::gt0100'\")");
		assertEquals("gt0100", code);
	}

    @Test
    public void shouldParseCodeWithDvCodedText() {
		String code = converter.parseCode(" new DvCodedText(\"Dubois and Dubois\",\"local\",\"gt0008\")");
		assertEquals("gt0008", code);
	}
	
	private GDLDroolsConverter converter;	
}
