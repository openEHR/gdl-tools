package se.cambio.openehr.controller.terminology.ts;

import org.openehr.rm.datatypes.text.CodePhrase;
import org.testng.annotations.Test;
import se.cambio.cm.util.exceptions.InvalidCodeException;

import java.util.HashSet;
import java.util.Set;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

public class ICD10Test extends TerminologyServiceTestBase {

    @Test
    public void testICD10Supported() {
        assertThat(terminologyService.isTerminologySupported(ICD10), is(true));
    }

    @Test
    public void testICD10SupportedCodePhrase() {
        CodePhrase cp = new CodePhrase(ICD10, "I01");
        boolean terminologySupported = terminologyService.isTerminologySupported(cp);
        assertThat(terminologySupported, equalTo(true));
        cp = new CodePhrase(ICD10, "I011");
        terminologySupported = terminologyService.isTerminologySupported(cp);
        assertThat(terminologySupported, equalTo(true));
    }

    @Test
    public void testTopGroupSingleMatch() throws Exception {
        CodePhrase c1 = new CodePhrase(ICD10, "I01");
        CodePhrase c2 = new CodePhrase(ICD10, "I01");
        boolean subclassOf = terminologyService.isSubclassOf(c1, c2);
        assertThat(subclassOf, equalTo(true));
    }

    @Test
    public void testSingleWrongParent() throws Exception {
        CodePhrase c1 = new CodePhrase(ICD10, "I01");
        CodePhrase c2 = new CodePhrase(ICD10, "I02");
        boolean subclassOf = terminologyService.isSubclassOf(c1, c2);
        assertThat(subclassOf, equalTo(false));
    }

    @Test
    public void testSingleBadCode2() throws Exception {
        CodePhrase c1 = new CodePhrase(ICD10, "I");
        CodePhrase c2 = new CodePhrase(ICD10, "I61");
        assertThat(terminologyService.isSubclassOf(c1, c2), equalTo(false));
    }

    @Test
    public void testTopGroupSingleMatchExtraDash() throws Exception {
        CodePhrase c1 = new CodePhrase(ICD10, "I61-");
        CodePhrase c2 = new CodePhrase(ICD10, "I61");
        assertThat(terminologyService.isSubclassOf(c1, c2), is(true));
    }

    @Test
    public void testTopGroupSingleMatchExtraDashChar() throws Exception {
        CodePhrase c1 = new CodePhrase(ICD10, "I61-P");
        CodePhrase c2 = new CodePhrase(ICD10, "I61");
        assertThat(terminologyService.isSubclassOf(c1, c2), is(true));
    }

    @Test
    public void testTopGroupSingleMatchExtraLetter() throws Exception {
        CodePhrase c1 = new CodePhrase(ICD10, "E106W");
        CodePhrase c2 = new CodePhrase(ICD10, "E106B");
        CodePhrase c3 = new CodePhrase(ICD10, "E10");
        assertThat(terminologyService.isSubclassOf(c1, c3), is(true));
        assertThat(terminologyService.isSubclassOf(c2, c3), is(true));
    }

    @Test
    public void testTopGroupSingleMatchFalse() throws Exception {
        CodePhrase c1 = new CodePhrase(ICD10, "I61");
        CodePhrase c2 = new CodePhrase(ICD10, "M20");
        assertThat(terminologyService.isSubclassOf(c1, c2), is(false));
    }

    @Test
    public void testTopGroupSingleMatchExtraDashFalse() throws Exception {
        CodePhrase c1 = new CodePhrase(ICD10, "I61-");
        CodePhrase c2 = new CodePhrase(ICD10, "M20");
        assertThat(terminologyService.isSubclassOf(c1, c2), is(false));
    }

    @Test
    public void testTopGroupSingleMatchExtraDashCharFalse() throws Exception {
        CodePhrase c1 = new CodePhrase(ICD10, "I61-P");
        CodePhrase c2 = new CodePhrase(ICD10, "M20");
        assertThat(terminologyService.isSubclassOf(c1, c2), is(false));
    }

    @Test
    public void testSubGroupSingleMatch() throws Exception {
        CodePhrase c1 = new CodePhrase(ICD10, "I489");
        CodePhrase c2 = new CodePhrase(ICD10, "I48.9");
        assertThat(terminologyService.isSubclassOf(c1, c2), is(true));
    }

    @Test
    public void testSubGroupSingleMatchExtraChar() throws Exception {
        CodePhrase c1 = new CodePhrase(ICD10, "I489B");
        CodePhrase c2 = new CodePhrase(ICD10, "I48.9");
        assertThat(terminologyService.isSubclassOf(c1, c2), is(true));
    }

    @Test
    public void testSubGroupSingleMatchFalse() throws Exception {
        CodePhrase c1 = new CodePhrase(ICD10, "I489");
        CodePhrase c2 = new CodePhrase(ICD10, "H31.9");
        assertThat(terminologyService.isSubclassOf(c1, c2), is(false));
    }

    @Test
    public void testSubGroupSingleMatchExtraCharFalse() throws Exception {
        CodePhrase c1 = new CodePhrase(ICD10, "I489B");
        CodePhrase c2 = new CodePhrase(ICD10, "B19");
        assertThat(terminologyService.isSubclassOf(c1, c2), is(false));
    }

    @Test
    public void testTopGroupSetMatch() throws Exception {
        CodePhrase c1 = new CodePhrase(ICD10, "I48");
        CodePhrase c2 = new CodePhrase(ICD10, "I50");
        CodePhrase c3 = new CodePhrase(ICD10, "I49");
        CodePhrase c4 = new CodePhrase(ICD10, "I48");
        Set<CodePhrase> codes = new HashSet<>();
        codes.add(c2);
        codes.add(c3);
        codes.add(c4);
        assertThat(terminologyService.isSubclassOf(c1, codes), is(true));
    }

    @Test
    public void testTopGroupSetMatchFalse() throws Exception {
        CodePhrase c1 = new CodePhrase(ICD10, "G20");
        CodePhrase c2 = new CodePhrase(ICD10, "I50");
        CodePhrase c3 = new CodePhrase(ICD10, "I49");
        CodePhrase c4 = new CodePhrase(ICD10, "I48");
        Set<CodePhrase> codes = new HashSet<>();
        codes.add(c2);
        codes.add(c3);
        codes.add(c4);
        assertThat(terminologyService.isSubclassOf(c1, codes), is(false));
    }

    @Test
    public void testSubGroupSetMatchTrue() throws Exception {
        CodePhrase c1 = new CodePhrase(ICD10, "I48.3");
        CodePhrase c2 = new CodePhrase(ICD10, "I50");
        CodePhrase c3 = new CodePhrase(ICD10, "I49");
        CodePhrase c4 = new CodePhrase(ICD10, "I48");
        Set<CodePhrase> codes = new HashSet<>();
        codes.add(c2);
        codes.add(c3);
        codes.add(c4);
        assertThat(terminologyService.isSubclassOf(c1, codes), is(true));
    }

    @Test
    public void testSubGroupSetMatchFalse() throws Exception {
        CodePhrase c1 = new CodePhrase(ICD10, "I44.1");
        CodePhrase c2 = new CodePhrase(ICD10, "I50");
        CodePhrase c3 = new CodePhrase(ICD10, "I49");
        CodePhrase c4 = new CodePhrase(ICD10, "I44.3");
        Set<CodePhrase> codes = new HashSet<>();
        codes.add(c2);
        codes.add(c3);
        codes.add(c4);
        assertThat(terminologyService.isSubclassOf(c1, codes), is(false));
    }
}