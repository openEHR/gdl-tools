package se.cambio.openehr.controller.session.data;

import org.testng.annotations.Test;
import se.cambio.cm.model.archetype.vo.OrdinalVO;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.mock;

public class OrdinalsTest {

    private static final String TEST_ARCHETYPE_ID = "testArchetypeId";
    private static final String TEST_TERMINOLOGY_ID = "TEST_TERMINOLOGY";


    @Test
    public void should_find_ordinal_in_element_id() {
        Ordinals ordinals = new Ordinals(mock(ArchetypeManager.class));
        ordinals.loadOrdinals(
                TEST_ARCHETYPE_ID,
                null,
                Arrays.asList(
                        OrdinalVO.builder()
                                .description("ordinalDescTest1a")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath1")
                                .lowerCardinality(1)
                                .terminology(TEST_TERMINOLOGY_ID)
                                .code("testCode1a")
                                .build(),
                        OrdinalVO.builder()
                                .description("ordinalDescTest1b")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath1")
                                .lowerCardinality(2)
                                .terminology(TEST_TERMINOLOGY_ID)
                                .code("testCode1b")
                                .build(),
                        OrdinalVO.builder()
                                .description("ordinalDescTest2a")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath2")
                                .lowerCardinality(1)
                                .terminology(TEST_TERMINOLOGY_ID)
                                .code("testCode2a")
                                .build()));
        OrdinalVO ordinalVO = ordinals.getOrdinalVO(null, TEST_ARCHETYPE_ID + "/archetypeElementPath1", "testCode1b");
        assertThat(ordinalVO.getDescription(), equalTo("ordinalDescTest1b"));
        ordinalVO = ordinals.getOrdinalVO(null, TEST_ARCHETYPE_ID + "/archetypeElementPath2", "testCode2a");
        assertThat(ordinalVO.getDescription(), equalTo("ordinalDescTest2a"));
        List<OrdinalVO> ordinalVOs = ordinals.getOrdinalVOs(null, TEST_ARCHETYPE_ID + "/archetypeElementPath1");
        assertThat(ordinalVOs.size(), equalTo(2));
        ordinalVOs = ordinals.getOrdinalVOs(null, TEST_ARCHETYPE_ID + "/archetypeElementPath2");
        assertThat(ordinalVOs.size(), equalTo(1));

    }

    @Test(expectedExceptions = RuntimeException.class)
    public void should_not_find_ordinal() {
        Ordinals ordinals = new Ordinals(mock(ArchetypeManager.class));
        ordinals.getOrdinalVO(null, TEST_ARCHETYPE_ID + "/archetypeElementPath1", "testCode1");
    }

    @Test
    public void should_find_ordinal_after_second_load() {
        Ordinals ordinals = new Ordinals(mock(ArchetypeManager.class));
        ordinals.loadOrdinals(
                TEST_ARCHETYPE_ID,
                null,
                Collections.singletonList(
                        OrdinalVO.builder()
                                .description("ordinalDescTest2a")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath2")
                                .lowerCardinality(2)
                                .terminology(TEST_TERMINOLOGY_ID)
                                .code("testCode2a")
                                .build()));
        ordinals.loadOrdinals(
                TEST_ARCHETYPE_ID,
                null,
                Arrays.asList(
                        OrdinalVO.builder()
                                .description("ordinalDescTest1a")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath1")
                                .lowerCardinality(1)
                                .terminology(TEST_TERMINOLOGY_ID)
                                .code("testCode1a")
                                .build(),
                        OrdinalVO.builder()
                                .description("ordinalDescTest1b")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath1")
                                .lowerCardinality(2)
                                .terminology(TEST_TERMINOLOGY_ID)
                                .code("testCode1b")
                                .build()));
        List<OrdinalVO> ordinalVOs = ordinals.getOrdinalVOs(null, TEST_ARCHETYPE_ID + "/archetypeElementPath1");
        assertThat(ordinalVOs.size(), equalTo(2));
    }

    @Test(expectedExceptions = RuntimeException.class)
    public void should_not_find_ordinal_after_second_load() {
        Ordinals ordinals = new Ordinals(mock(ArchetypeManager.class));
        ordinals.loadOrdinals(
                TEST_ARCHETYPE_ID,
                null,
                Collections.singletonList(
                        OrdinalVO.builder()
                                .description("ordinalDescTest2a")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath2")
                                .lowerCardinality(2)
                                .terminology(TEST_TERMINOLOGY_ID)
                                .code("testCode2a")
                                .build()));
        OrdinalVO ordinalVO = ordinals.getOrdinalVO(null, TEST_ARCHETYPE_ID + "/archetypeElementPath2", "testCode2a");
        assertThat(ordinalVO.getDescription(), equalTo("ordinalDescTest2a"));
        ordinals.loadOrdinals(
                TEST_ARCHETYPE_ID,
                null,
                Arrays.asList(
                        OrdinalVO.builder()
                                .description("ordinalDescTest1a")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath1")
                                .lowerCardinality(1)
                                .terminology(TEST_TERMINOLOGY_ID)
                                .code("testCode1a")
                                .build(),
                        OrdinalVO.builder()
                                .description("ordinalDescTest1b")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath1")
                                .lowerCardinality(2)
                                .terminology(TEST_TERMINOLOGY_ID)
                                .code("testCode1b")
                                .build()));
        ordinals.getOrdinalVO(null, TEST_ARCHETYPE_ID + "/archetypeElementPath2", "testCode2a");
    }
}