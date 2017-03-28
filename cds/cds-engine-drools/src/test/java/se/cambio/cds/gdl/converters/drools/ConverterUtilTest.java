package se.cambio.cds.gdl.converters.drools;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import se.cambio.cds.configuration.DroolsConfiguration;
import se.cambio.openehr.controller.session.data.ArchetypeManager;

import static org.junit.Assert.assertEquals;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = {DroolsConfiguration.class})
public class ConverterUtilTest {

	@Autowired
	ArchetypeManager archetypeManager;

    @Before
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
