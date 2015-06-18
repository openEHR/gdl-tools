package se.cambio.openehr.controller.terminology.ts;

import org.junit.Test;
import org.openehr.rm.datatypes.text.CodePhrase;
import se.cambio.openehr.util.exceptions.InvalidCodeException;

import java.util.HashSet;
import java.util.Set;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class ICD10Test extends TerminologyServiceTestBase {

	public void testICD10Supported() {
		assertTrue(ts.isTerminologySupported("ICD10"));
	}

	@Test
	public void testICD10SupportedCodePhrase() {
		CodePhrase cp = new CodePhrase("ICD10", "I64");
		assertTrue(ts.isTerminologySupported(cp));
		cp = new CodePhrase("ICD10", "I10X");
		assertTrue(ts.isTerminologySupported(cp));
	}

	@Test
	public void testTopGroupSingleMatch() throws Exception {
		CodePhrase c1 = new CodePhrase(ICD10, "I61");
		CodePhrase c2 = new CodePhrase(ICD10, "I61");
		assertTrue(ts.isSubclassOf(c1, c2));
	}

	@Test
	public void testSingleBadCode() throws Exception {
		try {
			CodePhrase c1 = new CodePhrase(ICD10, "IX");
			CodePhrase c2 = new CodePhrase(ICD10, "I61");
			ts.isSubclassOf(c1, c2);
		} catch(Exception e) {
			assertTrue("unexpected exception: " + e, e instanceof InvalidCodeException);
		}
	}

	@Test
	public void testSingleBadCode2() throws Exception {
		try {
			CodePhrase c1 = new CodePhrase(ICD10, "I");
			CodePhrase c2 = new CodePhrase(ICD10, "I61");
			ts.isSubclassOf(c1, c2);
		} catch(Exception e) {
			assertTrue("unexpected exception: " + e, e instanceof InvalidCodeException);
		}
	}

	@Test
	public void testTopGroupSingleMatchExtraDash() throws Exception {
		CodePhrase c1 = new CodePhrase(ICD10, "I61-");
		CodePhrase c2 = new CodePhrase(ICD10, "I61");
		assertTrue(ts.isSubclassOf(c1, c2));
	}

	@Test
	public void testTopGroupSingleMatchExtraDashChar() throws Exception {
		CodePhrase c1 = new CodePhrase(ICD10, "I61-P");
		CodePhrase c2 = new CodePhrase(ICD10, "I61");
		assertTrue(ts.isSubclassOf(c1, c2));
	}

	@Test
	public void testTopGroupSingleMatchExtraLetter() throws Exception {
		CodePhrase c1 = new CodePhrase(ICD10, "E106W");
		CodePhrase c2 = new CodePhrase(ICD10, "E106B");
		CodePhrase c3 = new CodePhrase(ICD10, "E10");
		assertTrue(ts.isSubclassOf(c1, c3));
		assertTrue(ts.isSubclassOf(c2, c3));
	}

	@Test
	public void testTopGroupSingleMatchFalse() throws Exception {
		CodePhrase c1 = new CodePhrase(ICD10, "I61");
		CodePhrase c2 = new CodePhrase(ICD10, "M20");
		assertFalse(ts.isSubclassOf(c1, c2));
	}

	@Test
	public void testTopGroupSingleMatchExtraDashFalse() throws Exception {
		CodePhrase c1 = new CodePhrase(ICD10, "I61-");
		CodePhrase c2 = new CodePhrase(ICD10, "M20");
		assertFalse(ts.isSubclassOf(c1, c2));
	}

	@Test
	public void testTopGroupSingleMatchExtraDashCharFalse() throws Exception {
		CodePhrase c1 = new CodePhrase(ICD10, "I61-P");
		CodePhrase c2 = new CodePhrase(ICD10, "M20");
		assertFalse(ts.isSubclassOf(c1, c2));
	}

	@Test
	public void testSubGroupSingleMatch() throws Exception {
		CodePhrase c1 = new CodePhrase(ICD10, "I489");
		CodePhrase c2 = new CodePhrase(ICD10, "I48.9");
		assertTrue(ts.isSubclassOf(c1, c2));
	}

	@Test
	public void testSubGroupSingleMatchExtraChar() throws Exception {
		CodePhrase c1 = new CodePhrase(ICD10, "I489B");
		CodePhrase c2 = new CodePhrase(ICD10, "I48.9");
		assertTrue(ts.isSubclassOf(c1, c2));
	}

	@Test
	public void testSubGroupSingleMatchFalse() throws Exception {
		CodePhrase c1 = new CodePhrase(ICD10, "I489");
		CodePhrase c2 = new CodePhrase(ICD10, "H31.9");
		assertFalse(ts.isSubclassOf(c1, c2));
	}

	@Test
	public void testSubGroupSingleMatchExtraCharFalse() throws Exception {
		CodePhrase c1 = new CodePhrase(ICD10, "I489B");
		CodePhrase c2 = new CodePhrase(ICD10, "B19");
		assertFalse(ts.isSubclassOf(c1, c2));
	}

	@Test
	public void testTopGroupSetMatch() throws Exception {
		CodePhrase c1 = new CodePhrase(ICD10, "I48");
		CodePhrase c2 = new CodePhrase(ICD10, "I50");
		CodePhrase c3 = new CodePhrase(ICD10, "I49");
		CodePhrase c4 = new CodePhrase(ICD10, "I48");
		Set<CodePhrase> codes = new HashSet<CodePhrase>();
		codes.add(c2);
		codes.add(c3);
		codes.add(c4);
		assertTrue(ts.isSubclassOf(c1, codes));
	}

	@Test
	public void testTopGroupSetMatchFalse() throws Exception {
		CodePhrase c1 = new CodePhrase(ICD10, "G20");
		CodePhrase c2 = new CodePhrase(ICD10, "I50");
		CodePhrase c3 = new CodePhrase(ICD10, "I49");
		CodePhrase c4 = new CodePhrase(ICD10, "I48");
		Set<CodePhrase> codes = new HashSet<CodePhrase>();
		codes.add(c2);
		codes.add(c3);
		codes.add(c4);
		assertFalse(ts.isSubclassOf(c1, codes));
	}

	@Test
	public void testSubGroupSetMatchTrue() throws Exception {
		CodePhrase c1 = new CodePhrase(ICD10, "I48.3");
		CodePhrase c2 = new CodePhrase(ICD10, "I50");
		CodePhrase c3 = new CodePhrase(ICD10, "I49");
		CodePhrase c4 = new CodePhrase(ICD10, "I48");
		Set<CodePhrase> codes = new HashSet<CodePhrase>();
		codes.add(c2);
		codes.add(c3);
		codes.add(c4);
		assertTrue(ts.isSubclassOf(c1, codes));
	}

	@Test
	public void testSubGroupSetMatchFalse() throws Exception {
		CodePhrase c1 = new CodePhrase(ICD10, "I44.1");
		CodePhrase c2 = new CodePhrase(ICD10, "I50");
		CodePhrase c3 = new CodePhrase(ICD10, "I49");
		CodePhrase c4 = new CodePhrase(ICD10, "I44.3");
		Set<CodePhrase> codes = new HashSet<CodePhrase>();
		codes.add(c2);
		codes.add(c3);
		codes.add(c4);
		assertFalse(ts.isSubclassOf(c1, codes));
	}
}