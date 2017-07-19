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
                        new OrdinalVO(null, "ordinalDescTest1a", null, TEST_ARCHETYPE_ID, null, "/archetypeElementPath1", 1, TEST_TERMINOLOGY_ID, "testCode1a"),
                        new OrdinalVO(null, "ordinalDescTest1b", null, TEST_ARCHETYPE_ID, null, "/archetypeElementPath1", 2, TEST_TERMINOLOGY_ID, "testCode1b"),
                        new OrdinalVO(null, "ordinalDescTest2a", null, TEST_ARCHETYPE_ID, null, "/archetypeElementPath2", 1, TEST_TERMINOLOGY_ID, "testCode2a")));
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
                        new OrdinalVO(null, "ordinalDescTest2a", null, TEST_ARCHETYPE_ID, null, "/archetypeElementPath2", 2, TEST_TERMINOLOGY_ID, "testCode2a")));
        ordinals.loadOrdinals(
                TEST_ARCHETYPE_ID,
                null,
                Arrays.asList(
                        new OrdinalVO(null, "ordinalDescTest1a", null, TEST_ARCHETYPE_ID, null, "/archetypeElementPath1", 1, TEST_TERMINOLOGY_ID, "testCode1a"),
                        new OrdinalVO(null, "ordinalDescTest1b", null, TEST_ARCHETYPE_ID, null, "/archetypeElementPath1", 2, TEST_TERMINOLOGY_ID, "testCode1b")));
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
                        new OrdinalVO(null, "ordinalDescTest2a", null, TEST_ARCHETYPE_ID, null, "/archetypeElementPath2", 2, TEST_TERMINOLOGY_ID, "testCode2a")));
        OrdinalVO ordinalVO = ordinals.getOrdinalVO(null, TEST_ARCHETYPE_ID + "/archetypeElementPath2", "testCode2a");
        assertThat(ordinalVO.getDescription(), equalTo("ordinalDescTest2a"));
        ordinals.loadOrdinals(
                TEST_ARCHETYPE_ID,
                null,
                Arrays.asList(
                        new OrdinalVO(null, "ordinalDescTest1a", null, TEST_ARCHETYPE_ID, null, "/archetypeElementPath1", 1, TEST_TERMINOLOGY_ID, "testCode1a"),
                        new OrdinalVO(null, "ordinalDescTest1b", null, TEST_ARCHETYPE_ID, null, "/archetypeElementPath1", 2, TEST_TERMINOLOGY_ID, "testCode1b")));
        ordinals.getOrdinalVO(null, TEST_ARCHETYPE_ID + "/archetypeElementPath2", "testCode2a");
    }
}