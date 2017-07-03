package se.cambio.openehr.controller.terminology.ts;

import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.testng.annotations.Test;
import se.cambio.cm.util.exceptions.InvalidCodeException;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;


public class MultiLanguageTerminologyTest extends TerminologyServiceTestBase {

    private static String LANG_TERMINOLOGY = "ISO_639-1";
    private static CodePhrase esLangCodePhrase = new CodePhrase(LANG_TERMINOLOGY,"es");
    private static CodePhrase svLangCodePhrase = new CodePhrase(LANG_TERMINOLOGY,"sv");

    @Test
    public void testTerminologySupported() {
        assertThat(terminologyService.isTerminologySupported(MULTI_LANG_TEST_TERMINOLOGY), is(true));
    }

    @Test
    public void testSimpleSupport() {
        CodePhrase cp = new CodePhrase(MULTI_LANG_TEST_TERMINOLOGY, "test1");
        assertThat(terminologyService.isTerminologySupported(cp), is(true));
        cp = new CodePhrase(MULTI_LANG_TEST_TERMINOLOGY, "test2");
        assertThat(terminologyService.isTerminologySupported(cp), is(true));
    }

    @Test
    public void testTopGroupSingleMatch() {
        CodePhrase c1 = new CodePhrase(MULTI_LANG_TEST_TERMINOLOGY, "test2");
        CodePhrase c2 = new CodePhrase(MULTI_LANG_TEST_TERMINOLOGY, "test1");
        assertThat(terminologyService.isSubclassOf(c1, c2), is(true));
    }

    @Test
    public void testSimpleTranslations() throws Exception {
        CodePhrase c1 = new CodePhrase(MULTI_LANG_TEST_TERMINOLOGY, "test2");
        DvCodedText dvCodedText = new DvCodedText("Name1", c1);
        assertThat(terminologyService.translate(dvCodedText, esLangCodePhrase), equalTo(new DvCodedText("Name2_es", c1)));
        assertThat(terminologyService.translate(dvCodedText, svLangCodePhrase), equalTo(new DvCodedText("Name2_sv", c1)));
    }
}