package se.cambio.openehr.controller.session.data;

import org.testng.annotations.Test;
import se.cambio.cm.model.archetype.vo.CodedTextVO;

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
                        CodedTextVO.builder()
                                .description("codedTextDescTest1a")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath1")
                                .terminology(TEST_TERMINOLOGY_ID)
                                .code("testCode1a").build(),
                        CodedTextVO.builder()
                                .description("codedTextDescTest1b")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath1")
                                .terminology(TEST_TERMINOLOGY_ID)
                                .code("testCode1b").build(),
                        CodedTextVO.builder()
                                .description("codedTextDescTest2a")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath2")
                                .terminology(TEST_TERMINOLOGY_ID)
                                .code("testCode2a").build()));
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
                        CodedTextVO.builder()
                                .description("codedTextDescTest2a")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath2")
                                .terminology(TEST_TERMINOLOGY_ID)
                                .code("testCode2a").build()));
        codedTexts.loadCodedTexts(
                TEST_ARCHETYPE_ID,
                null,
                Arrays.asList(
                        CodedTextVO.builder()
                                .description("codedTextDescTest1a")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath1")
                                .terminology(TEST_TERMINOLOGY_ID)
                                .code("testCode1a").build(),
                        CodedTextVO.builder()
                                .description("codedTextDescTest1b")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath1")
                                .terminology(TEST_TERMINOLOGY_ID)
                                .code("testCode1b").build()));
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
                        CodedTextVO.builder()
                                .description("codedTextDescTest2a")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath2")
                                .terminology(TEST_TERMINOLOGY_ID)
                                .code("testCode2a").build()));
        CodedTextVO codedTextVO = codedTexts.getCodedTextVO(null, TEST_ARCHETYPE_ID + "/archetypeElementPath2", "testCode2a");
        assertThat(codedTextVO.getDescription(), equalTo("codedTextDescTest2a"));
        codedTexts.loadCodedTexts(
                TEST_ARCHETYPE_ID,
                null,
                Arrays.asList(
                        CodedTextVO.builder()
                                .description("codedTextDescTest1a")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath1")
                                .terminology(TEST_TERMINOLOGY_ID)
                                .code("testCode1a").build(),
                        CodedTextVO.builder()
                                .description("codedTextDescTest1b")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath1")
                                .terminology(TEST_TERMINOLOGY_ID)
                                .code("testCode1b").build()));
        codedTexts.getCodedTextVO(null, TEST_ARCHETYPE_ID + "/archetypeElementPath2", "testCode2a");
    }
}