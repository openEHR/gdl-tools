package se.cambio.openehr.controller.terminology.ts;

import org.junit.Test;
import org.openehr.rm.datatypes.text.CodePhrase;
import se.cambio.openehr.util.exceptions.InvalidCodeException;

import java.util.HashSet;
import java.util.Set;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class PluginTerminologyTest extends TerminologyServiceTestBase {

	public void testTEST_TERMINOLOGYSupported() {
		assertTrue(terminologyService.isTerminologySupported("TEST-TERMINOLOGY"));
	}

	@Test
	public void testTEST_TERMINOLOGYSupportedCodePhrase() {
		CodePhrase cp = new CodePhrase("TEST-TERMINOLOGY", "test1");
		assertTrue(terminologyService.isTerminologySupported(cp));
	}

	@Test
	public void testSelfSingleMatch() throws Exception {
		CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test1");
		CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test1");
		assertTrue(terminologyService.isSubclassOf(c1, c2));
	}

	@Test
	public void testSingleBadCode() throws Exception {
		try {
			CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "tist1");
			CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test1");
			terminologyService.isSubclassOf(c1, c2);
		} catch(Exception e) {
			assertTrue("unexpected exception: " + e, e instanceof InvalidCodeException);
		}
	}

	@Test
	public void testSingleBadCode2() throws Exception {
		try {
			CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test1");
			CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "tist1");
			terminologyService.isSubclassOf(c1, c2);
		} catch(Exception e) {
			assertTrue("unexpected exception: " + e, e instanceof InvalidCodeException);
		}
	}

	@Test
	public void testTopGroupSingleMatchFalse() throws Exception {
		CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test1");
		CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test2");
		assertFalse(terminologyService.isSubclassOf(c1, c2));
	}

	@Test
	public void testTopGroupSingleMatch() throws Exception {
		CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test2");
		CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test1");
		assertTrue(terminologyService.isSubclassOf(c1, c2));
	}

	@Test
	public void testSubGroupSingleMatch() throws Exception {
		CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test5");
		CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test1");
		assertTrue(terminologyService.isSubclassOf(c1, c2));
	}

	@Test
	public void testTopGroupSetMatch() throws Exception {
		CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test5");
		CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test2");
		CodePhrase c3 = new CodePhrase(TEST_TERMINOLOGY, "test3");
		CodePhrase c4 = new CodePhrase(TEST_TERMINOLOGY, "test4");
		Set<CodePhrase> codes = new HashSet<CodePhrase>();
		codes.add(c2);
		codes.add(c3);
		codes.add(c4);
		assertTrue(terminologyService.isSubclassOf(c1, codes));
	}

	@Test
	public void testTopGroupSetMatchFalse() throws Exception {
		CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test1");
		CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test2");
		CodePhrase c3 = new CodePhrase(TEST_TERMINOLOGY, "test3");
		CodePhrase c4 = new CodePhrase(TEST_TERMINOLOGY, "test4");
		Set<CodePhrase> codes = new HashSet<CodePhrase>();
		codes.add(c2);
		codes.add(c3);
		codes.add(c4);
		assertFalse(terminologyService.isSubclassOf(c1, codes));
	}

	@Test
	public void testSubGroupSetMatchTrue() throws Exception {
		CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test5");
		CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test1");
		CodePhrase c3 = new CodePhrase(TEST_TERMINOLOGY, "test2");
		CodePhrase c4 = new CodePhrase(TEST_TERMINOLOGY, "test3");
		Set<CodePhrase> codes = new HashSet<CodePhrase>();
		codes.add(c2);
		codes.add(c3);
		codes.add(c4);
		assertTrue(terminologyService.isSubclassOf(c1, codes));
	}

	private static final String TEST_TERMINOLOGY = "TEST-TERMINOLOGY";
}
