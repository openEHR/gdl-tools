package se.cambio.openehr.controller.terminology.ts;

import org.openehr.rm.datatypes.text.CodePhrase;
import org.testng.annotations.Test;
import se.cambio.cm.util.exceptions.InvalidCodeException;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;


public class SimpleTerminologyTest extends TerminologyServiceTestBase {

    @Test
    public void testTerminologySupported() {
        assertThat(terminologyService.isTerminologySupported(TEST_TERMINOLOGY), is(true));
    }

    @Test
    public void testICD10SupportedCodePhrase() {
        CodePhrase cp = new CodePhrase(TEST_TERMINOLOGY, "test1");
        assertThat(terminologyService.isTerminologySupported(cp), is(true));
        cp = new CodePhrase(TEST_TERMINOLOGY, "test2");
        assertThat(terminologyService.isTerminologySupported(cp), is(true));
    }

    @Test
    public void testTopGroupSingleMatch() throws Exception {
        CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test2");
        CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test1");
        assertThat(terminologyService.isSubclassOf(c1, c2), is(true));
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
        assertThat(terminologyService.isSubclassOf(c1, c2), is(true));
    }
}