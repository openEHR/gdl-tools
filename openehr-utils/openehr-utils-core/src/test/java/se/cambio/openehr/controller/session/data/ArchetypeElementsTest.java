package se.cambio.openehr.controller.session.data;

import org.testng.annotations.Test;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;

import java.util.Arrays;
import java.util.Collections;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.mock;

public class ArchetypeElementsTest {

    private static final String TEST_ARCHETYPE_ID = "testArchetypeId";

    @Test
    public void should_find_archetype_element_in_element_id() {
        ArchetypeElements archetypeElements = new ArchetypeElements(mock(ArchetypeManager.class));
        archetypeElements.loadArchetypeElements(
                TEST_ARCHETYPE_ID,
                null,
                Arrays.asList(
                        ArchetypeElementVO.builder()
                                .description("elementIdTest1")
                                .type("DV_TEXT")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath1")
                                .build(),
                        ArchetypeElementVO.builder()
                                .description("elementIdTest2")
                                .type("DV_TEXT")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath2")
                                .build()));
        ArchetypeElementVO archetypeElementVO = archetypeElements.getArchetypeElement(null, TEST_ARCHETYPE_ID + "/archetypeElementPath1");
        assertThat(archetypeElementVO.getDescription(), equalTo("elementIdTest1"));
        archetypeElementVO = archetypeElements.getArchetypeElement(null, TEST_ARCHETYPE_ID + "/archetypeElementPath2");
        assertThat(archetypeElementVO.getDescription(), equalTo("elementIdTest2"));

    }

    @Test(expectedExceptions = RuntimeException.class)
    public void should_not_find_archetype_element() {
        ArchetypeElements archetypeElements = new ArchetypeElements(mock(ArchetypeManager.class));
        archetypeElements.getArchetypeElement(null, TEST_ARCHETYPE_ID + "/archetypeElementPath1");
    }

    @Test
    public void should_find_archetype_element_after_second_load() {
        ArchetypeElements archetypeElements = new ArchetypeElements(mock(ArchetypeManager.class));
        archetypeElements.loadArchetypeElements(
                TEST_ARCHETYPE_ID,
                null,
                Collections.singletonList(
                        ArchetypeElementVO.builder()
                                .description("elementIdTest1")
                                .type("DV_TEXT")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath1")
                                .build()));
        archetypeElements.loadArchetypeElements(
                TEST_ARCHETYPE_ID,
                null,
                Collections.singletonList(
                        ArchetypeElementVO.builder()
                                .description("elementIdTest2")
                                .type("DV_TEXT")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath2")
                                .build()));
        ArchetypeElementVO archetypeElementVO = archetypeElements.getArchetypeElement(null, TEST_ARCHETYPE_ID + "/archetypeElementPath2");
        assertThat(archetypeElementVO.getDescription(), equalTo("elementIdTest2"));
    }

    @Test(expectedExceptions = RuntimeException.class)
    public void should_not_find_archetype_element_after_second_load() {
        ArchetypeElements archetypeElements = new ArchetypeElements(mock(ArchetypeManager.class));
        archetypeElements.loadArchetypeElements(
                TEST_ARCHETYPE_ID,
                null,
                Collections.singletonList(
                        ArchetypeElementVO.builder()
                                .description("elementIdTest1")
                                .type("DV_TEXT")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath1")
                                .build()));
        archetypeElements.loadArchetypeElements(
                TEST_ARCHETYPE_ID,
                null,
                Collections.singletonList(
                        ArchetypeElementVO.builder()
                                .description("elementIdTest2")
                                .type("DV_TEXT")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/archetypeElementPath2")
                                .build()));
        archetypeElements.getArchetypeElement(null, TEST_ARCHETYPE_ID + "/archetypeElementPath1");
    }
}