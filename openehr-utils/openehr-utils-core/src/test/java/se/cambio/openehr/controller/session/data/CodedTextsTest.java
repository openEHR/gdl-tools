package se.cambio.openehr.controller.session.data;

import org.testng.annotations.Test;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.cm.model.archetype.vo.CodedTextVO;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.mock;

public class CodedTextsTest {

    private static final String TEST_ARCHETYPE_ID = "testArchetypeId";
    private static final String TEST_TERMINOLOGY_ID = "TEST_TERMINOLOGY";


    @Test
    public void should_find_coded_text_in_element_id() {
        CodedTexts codedTexts = new CodedTexts(mock(ArchetypeManager.class));
        codedTexts.loadCodedTexts(
                TEST_ARCHETYPE_ID,
                null,
                Arrays.asList(
                        new CodedTextVO(null, "codedTextDescTest1a", null, TEST_ARCHETYPE_ID, null, "/archetypeElementPath1", TEST_TERMINOLOGY_ID, "testCode1a"),
                        new CodedTextVO(null, "codedTextDescTest1b", null, TEST_ARCHETYPE_ID, null, "/archetypeElementPath1", TEST_TERMINOLOGY_ID, "testCode1b"),
                        new CodedTextVO(null, "codedTextDescTest2a", null, TEST_ARCHETYPE_ID, null, "/archetypeElementPath2", TEST_TERMINOLOGY_ID, "testCode2a")));
        CodedTextVO codedTextVO = codedTexts.getCodedTextVO(null, TEST_ARCHETYPE_ID + "/archetypeElementPath1", "testCode1b");
        assertThat(codedTextVO.getDescription(), equalTo("codedTextDescTest1b"));
        codedTextVO = codedTexts.getCodedTextVO(null, TEST_ARCHETYPE_ID + "/archetypeElementPath2", "testCode2a");
        assertThat(codedTextVO.getDescription(), equalTo("codedTextDescTest2a"));
        List<CodedTextVO> codedTextVOs = codedTexts.getCodedTextVOs(null, TEST_ARCHETYPE_ID + "/archetypeElementPath1");
        assertThat(codedTextVOs.size(), equalTo(2));
        codedTextVOs = codedTexts.getCodedTextVOs(null, TEST_ARCHETYPE_ID + "/archetypeElementPath2");
        assertThat(codedTextVOs.size(), equalTo(1));

    }

    @Test(expectedExceptions = RuntimeException.class)
    public void should_not_find_coded_text() {
        CodedTexts codedTexts = new CodedTexts(mock(ArchetypeManager.class));
        codedTexts.getCodedTextVO(null, TEST_ARCHETYPE_ID + "/archetypeElementPath1", "testCode1");
    }

    @Test
    public void should_find_coded_text_after_second_load() {
        CodedTexts codedTexts = new CodedTexts(mock(ArchetypeManager.class));
        codedTexts.loadCodedTexts(
                TEST_ARCHETYPE_ID,
                null,
                Collections.singletonList(
                        new CodedTextVO(null, "codedTextDescTest2a", null, TEST_ARCHETYPE_ID, null, "/archetypeElementPath2", TEST_TERMINOLOGY_ID, "testCode2a")));
        codedTexts.loadCodedTexts(
                TEST_ARCHETYPE_ID,
                null,
                Arrays.asList(
                        new CodedTextVO(null, "codedTextDescTest1a", null, TEST_ARCHETYPE_ID, null, "/archetypeElementPath1", TEST_TERMINOLOGY_ID, "testCode1a"),
                        new CodedTextVO(null, "codedTextDescTest1b", null, TEST_ARCHETYPE_ID, null, "/archetypeElementPath1", TEST_TERMINOLOGY_ID, "testCode1b")));
        List<CodedTextVO> codedTextVOs = codedTexts.getCodedTextVOs(null, TEST_ARCHETYPE_ID + "/archetypeElementPath1");
        assertThat(codedTextVOs.size(), equalTo(2));
    }

    @Test(expectedExceptions = RuntimeException.class)
    public void should_not_find_coded_text_after_second_load() {
        CodedTexts codedTexts = new CodedTexts(mock(ArchetypeManager.class));
        codedTexts.loadCodedTexts(
                TEST_ARCHETYPE_ID,
                null,
                Collections.singletonList(
                        new CodedTextVO(null, "codedTextDescTest2a", null, TEST_ARCHETYPE_ID, null, "/archetypeElementPath2", TEST_TERMINOLOGY_ID, "testCode2a")));
        CodedTextVO codedTextVO = codedTexts.getCodedTextVO(null, TEST_ARCHETYPE_ID + "/archetypeElementPath2", "testCode2a");
        assertThat(codedTextVO.getDescription(), equalTo("codedTextDescTest2a"));
        codedTexts.loadCodedTexts(
                TEST_ARCHETYPE_ID,
                null,
                Arrays.asList(
                        new CodedTextVO(null, "codedTextDescTest1a", null, TEST_ARCHETYPE_ID, null, "/archetypeElementPath1", TEST_TERMINOLOGY_ID, "testCode1a"),
                        new CodedTextVO(null, "codedTextDescTest1b", null, TEST_ARCHETYPE_ID, null, "/archetypeElementPath1", TEST_TERMINOLOGY_ID, "testCode1b")));
        codedTexts.getCodedTextVO(null, TEST_ARCHETYPE_ID + "/archetypeElementPath2", "testCode2a");
    }
}