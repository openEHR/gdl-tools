package se.cambio.openehr.controller.terminology.ts;

import org.openehr.rm.datatypes.text.CodePhrase;
import org.testng.annotations.Test;
import se.cambio.openehr.util.exceptions.InvalidCodeException;

import java.util.HashSet;
import java.util.Set;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class ICD10SETest extends TerminologyServiceTestBase {

	public void testICD10Supported() {
		assertTrue(terminologyService.isTerminologySupported("ICD10"));
	}

	@Test
	public void testICD10SupportedCodePhrase() {
		CodePhrase cp = new CodePhrase(ICD10SE, "I64");
		assertTrue(terminologyService.isTerminologySupported(cp));
		cp = new CodePhrase(ICD10, "I10X");
		assertTrue(terminologyService.isTerminologySupported(cp));
	}

	@Test
	public void testSubGroupSetMatchTrue() throws Exception {
		CodePhrase c1 = new CodePhrase(ICD10SE, "I48.3");
		CodePhrase c2 = new CodePhrase(ICD10SE, "I50");
		CodePhrase c3 = new CodePhrase(ICD10SE, "I49");
		CodePhrase c4 = new CodePhrase(ICD10SE, "I48");
		Set<CodePhrase> codes = new HashSet<CodePhrase>();
		codes.add(c2);
		codes.add(c3);
		codes.add(c4);
		assertTrue(terminologyService.isSubclassOf(c1, codes));
	}

	@Test
	public void testSubGroupSetMatchFalse() throws Exception {
		CodePhrase c1 = new CodePhrase(ICD10SE, "I44.1");
		CodePhrase c2 = new CodePhrase(ICD10SE, "I50");
		CodePhrase c3 = new CodePhrase(ICD10SE, "I49");
		CodePhrase c4 = new CodePhrase(ICD10SE, "I44.3");
		Set<CodePhrase> codes = new HashSet<CodePhrase>();
		codes.add(c2);
		codes.add(c3);
		codes.add(c4);
		assertFalse(terminologyService.isSubclassOf(c1, codes));
	}
}