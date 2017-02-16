package se.cambio.openehr.controller.terminology.ts;

import org.openehr.rm.datatypes.text.CodePhrase;
import org.testng.annotations.ExpectedExceptions;
import org.testng.annotations.Test;
import se.cambio.openehr.util.exceptions.InvalidCodeException;

import java.util.HashSet;
import java.util.Set;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class SimpleTerminologyTest extends TerminologyServiceTestBase {

    public void testTerminologySupported() {
        assertTrue(terminologyService.isTerminologySupported(TEST_TERMINOLOGY));
    }

    @Test
    public void testICD10SupportedCodePhrase() {
        CodePhrase cp = new CodePhrase(TEST_TERMINOLOGY, "test1");
        assertTrue(terminologyService.isTerminologySupported(cp));
        cp = new CodePhrase(TEST_TERMINOLOGY, "test2");
        assertTrue(terminologyService.isTerminologySupported(cp));
    }

    @Test
    public void testTopGroupSingleMatch() throws Exception {
        CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test2");
        CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test1");
        assertTrue(terminologyService.isSubclassOf(c1, c2));
    }

    @Test(expectedExceptions = InvalidCodeException.class)
    public void testSingleBadCode() throws Exception {
        CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test1");
        CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test6");
        terminologyService.isSubclassOf(c1, c2);
    }

    @Test
    public void testCorrelationMatch() throws Exception {
        CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test5");
        CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test1");
        assertTrue(terminologyService.isSubclassOf(c1, c2));
    }
}