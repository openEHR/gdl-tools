package se.cambio.openehr.controller.terminology.ts;

import org.openehr.rm.datatypes.text.CodePhrase;
import org.testng.annotations.Test;
import se.cambio.cm.util.exceptions.InvalidCodeException;
import se.cambio.cm.util.exceptions.UnsupportedTerminologyException;

import java.util.HashSet;
import java.util.Set;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

public class PluginTerminologyTest extends TerminologyServiceTestBase {

    @Test
    public void testTEST_TERMINOLOGYSupported() {
        assertThat(terminologyService.isTerminologySupported("TEST-TERMINOLOGY"), is(true));
    }

    @Test
    public void testTEST_TERMINOLOGYSupportedCodePhrase() {
        CodePhrase cp = new CodePhrase("TEST-TERMINOLOGY", "test1");
        assertThat(terminologyService.isTerminologySupported(cp), is(true));
    }

    @Test
    public void testSelfSingleMatch() throws Exception {
        CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test1");
        CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test1");
        assertThat(terminologyService.isSubclassOf(c1, c2), is(true));
    }

    @Test(expectedExceptions = UnsupportedTerminologyException.class)
    public void testBadTerminology() {
        CodePhrase c1 = new CodePhrase("BAD_TERMINOLOGY", "test1");
        CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "tist1");
        terminologyService.isSubclassOf(c1, c2);
    }

    @Test(expectedExceptions = InvalidCodeException.class)
    public void testSingleBadCode() {
        CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "tist1");
        CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test1");
        terminologyService.isSubclassOf(c1, c2);
    }

    @Test(expectedExceptions = InvalidCodeException.class)
    public void testSingleBadCode2() {
        CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test1");
        CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "tist1");
        terminologyService.isSubclassOf(c1, c2);
    }

    @Test
    public void testTopGroupSingleMatchFalse() throws Exception {
        CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test1");
        CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test2");
        assertThat(terminologyService.isSubclassOf(c1, c2), is(false));
    }

    @Test
    public void testTopGroupSingleMatch() throws Exception {
        CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test2");
        CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test1");
        assertThat(terminologyService.isSubclassOf(c1, c2), is(true));
    }

    @Test
    public void testSubGroupSingleMatch() throws Exception {
        CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test5");
        CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test1");
        assertThat(terminologyService.isSubclassOf(c1, c2), is(true));
    }

    @Test
    public void testTopGroupSetMatch() throws Exception {
        CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test5");
        CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test2");
        CodePhrase c3 = new CodePhrase(TEST_TERMINOLOGY, "test3");
        CodePhrase c4 = new CodePhrase(TEST_TERMINOLOGY, "test4");
        Set<CodePhrase> codes = new HashSet<>();
        codes.add(c2);
        codes.add(c3);
        codes.add(c4);
        assertThat(terminologyService.isSubclassOf(c1, codes), is(true));
    }

    @Test
    public void testTopGroupSetMatchFalse() throws Exception {
        CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test1");
        CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test2");
        CodePhrase c3 = new CodePhrase(TEST_TERMINOLOGY, "test3");
        CodePhrase c4 = new CodePhrase(TEST_TERMINOLOGY, "test4");
        Set<CodePhrase> codes = new HashSet<>();
        codes.add(c2);
        codes.add(c3);
        codes.add(c4);
        assertThat(terminologyService.isSubclassOf(c1, codes), is(false));
    }

    @Test
    public void testSubGroupSetMatchTrue() throws Exception {
        CodePhrase c1 = new CodePhrase(TEST_TERMINOLOGY, "test5");
        CodePhrase c2 = new CodePhrase(TEST_TERMINOLOGY, "test1");
        CodePhrase c3 = new CodePhrase(TEST_TERMINOLOGY, "test2");
        CodePhrase c4 = new CodePhrase(TEST_TERMINOLOGY, "test3");
        Set<CodePhrase> codes = new HashSet<>();
        codes.add(c2);
        codes.add(c3);
        codes.add(c4);
        assertThat(terminologyService.isSubclassOf(c1, codes), is(true));
    }

    private static final String TEST_TERMINOLOGY = "TEST-TERMINOLOGY";
}
