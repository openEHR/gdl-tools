package se.cambio.openehr.controller.terminology.ts;

import java.util.HashSet;
import java.util.Set;

import org.openehr.rm.datatypes.text.CodePhrase;

import se.cambio.openehr.controller.terminology.TerminologyServiceImpl;
import se.cambio.openehr.util.exceptions.InvalidCodeException;

public class PluginTerminologyTest extends TerminologyServiceTestBase {

	public PluginTerminologyTest() throws Exception {
		super();
	}
	
	public void testTEST_TERMINOLOGYSupported() {
		assertTrue(ts.isTerminologySupported("TEST-TERMINOLOGY"));
	}
	
	public void testTEST_TERMINOLOGYSupportedCodePhrase() {
		CodePhrase cp = new CodePhrase("TEST-TERMINOLOGY", "test1");
		assertTrue(((TerminologyServiceImpl) ts).isTerminologySupported(cp));
	}
	
	public void testSelfSingleMatch() throws Exception {
		CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test1");
		CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test1");		
		assertTrue(ts.isSubclassOf(c1, c2));
	}
	
	public void testSingleBadCode() throws Exception {
		try {
			CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "tist1");
			CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test1");
			ts.isSubclassOf(c1, c2);
		} catch(Exception e) {
			assertTrue("unexpected exception: " + e, e instanceof InvalidCodeException);
		}
	}
	
	public void testSingleBadCode2() throws Exception {
		try {
			CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test1");
			CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "tist1");
			ts.isSubclassOf(c1, c2);
		} catch(Exception e) {
			assertTrue("unexpected exception: " + e, e instanceof InvalidCodeException);
		}
	}
	
	
	public void testTopGroupSingleMatchFalse() throws Exception {
		CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test1");
		CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test2");		
		assertFalse(ts.isSubclassOf(c1, c2));
	}
	
	public void testTopGroupSingleMatch() throws Exception {
		CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test2");
		CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test1");		
		assertTrue(ts.isSubclassOf(c1, c2));
	}
	
	public void testSubGroupSingleMatch() throws Exception {
		CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test5");
		CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test1");		
		assertTrue(ts.isSubclassOf(c1, c2));
	}
	
	
	public void testTopGroupSetMatch() throws Exception {
		CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test5");
		CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test2");
		CodePhrase c3 = new CodePhrase(TEST_TERMINOLOGY, "test3");
		CodePhrase c4 = new CodePhrase(TEST_TERMINOLOGY, "test4");
		Set<CodePhrase> codes = new HashSet<CodePhrase>();
		codes.add(c2);
		codes.add(c3);
		codes.add(c4);		
		assertTrue(ts.isSubclassOf(c1, codes));
	}
	
	public void testTopGroupSetMatchFalse() throws Exception {
		CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test1");
		CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test2");
		CodePhrase c3 = new CodePhrase(TEST_TERMINOLOGY, "test3");
		CodePhrase c4 = new CodePhrase(TEST_TERMINOLOGY, "test4");
		Set<CodePhrase> codes = new HashSet<CodePhrase>();
		codes.add(c2);
		codes.add(c3);
		codes.add(c4);		
		assertFalse(ts.isSubclassOf(c1, codes));
	}
	
	public void testSubGroupSetMatchTrue() throws Exception {
		CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test5");
		CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test1");
		CodePhrase c3 = new CodePhrase(TEST_TERMINOLOGY, "test2");
		CodePhrase c4 = new CodePhrase(TEST_TERMINOLOGY, "test3");
		Set<CodePhrase> codes = new HashSet<CodePhrase>();
		codes.add(c2);
		codes.add(c3);
		codes.add(c4);		
		assertTrue(ts.isSubclassOf(c1, codes));
	}
	
	private static final String TEST_TERMINOLOGY = "TEST-TERMINOLOGY";
}
