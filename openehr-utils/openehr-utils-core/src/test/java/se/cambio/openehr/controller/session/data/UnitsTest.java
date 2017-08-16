package se.cambio.openehr.controller.session.data;

import org.testng.annotations.Test;
import se.cambio.cm.model.archetype.vo.UnitVO;

import java.util.Arrays;
import java.util.Collection;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

public class UnitsTest {


    @Test
    public void should_find_units_in_element_id() {
        Units units = new Units();
        units.loadUnits(
                "testArchetype",
                null,
                Arrays.asList(
                        UnitVO.builder()
                                .idElement("elementIdTest1")
                                .unit("cm")
                                .build(),
                        UnitVO.builder()
                                .idElement("elementIdTest1")
                                .unit("m")
                                .build()));
        Collection<String> unitsAtElement1 = units.getUnits(null, "elementIdTest1");
        assertThat(unitsAtElement1.size(), equalTo(2));
        assertThat(unitsAtElement1, containsInAnyOrder("cm", "m"));

    }

    @Test
    public void should_find_unique_units_in_element_id() {
        Units units = new Units();
        units.loadUnits(
                "testArchetype",
                null,
                Arrays.asList(
                        UnitVO.builder()
                                .idElement("elementIdTest1")
                                .unit("cm")
                                .build(),
                        UnitVO.builder()
                                .idElement("elementIdTest1")
                                .unit("m")
                                .build(),
                        UnitVO.builder()
                                .idElement("elementIdTest1")
                                .unit("m")
                                .build()));
        Collection<String> unitsAtElement1 = units.getUnits(null, "elementIdTest1");
        assertThat(unitsAtElement1.size(), equalTo(2));
        assertThat(unitsAtElement1, containsInAnyOrder("cm", "m"));
    }

    @Test
    public void should_find_units_in_template_id() {
        Units units = new Units();
        units.loadUnits(
                "testArchetype",
                null,
                Arrays.asList(
                        UnitVO.builder()
                                .idElement("elementIdTest1")
                                .unit("cm")
                                .build(),
                        UnitVO.builder()
                                .idElement("elementIdTest1")
                                .unit("m")
                                .build(),
                        UnitVO.builder()
                                .idElement("elementIdTest1")
                                .unit("m")
                                .build(),
                        UnitVO.builder()
                                .idTemplate("templateIdTest1")
                                .idElement("elementIdTest1")
                                .unit("dl")
                                .build(),
                        UnitVO.builder()
                                .idTemplate("templateIdTest1")
                                .idElement("elementIdTest1")
                                .unit("l")
                                .build(),
                        UnitVO.builder()
                                .idElement("elementIdTest2")
                                .unit("g")
                                .build(),
                        UnitVO.builder()
                                .idElement("elementIdTest2")
                                .unit("kg")
                                .build()));
        Collection<String> unitsAtElement1 = units.getUnits("templateIdTest1", "elementIdTest1");
        assertThat(unitsAtElement1.size(), equalTo(2));
        assertThat(unitsAtElement1, containsInAnyOrder("dl", "l"));
    }

    @Test
    public void should_find_units_in_second_element_id() {
        Units units = new Units();
        units.loadUnits(
                "testArchetype",
                null,
                Arrays.asList(
                        UnitVO.builder()
                                .idElement("elementIdTest1")
                                .unit("cm")
                                .build(),
                        UnitVO.builder()
                                .idElement("elementIdTest1")
                                .unit("m")
                                .build(),
                        UnitVO.builder()
                                .idElement("elementIdTest1")
                                .unit("m")
                                .build(),
                        UnitVO.builder()
                                .idElement("elementIdTest1")
                                .unit("cm")
                                .build(),
                        UnitVO.builder()
                                .idTemplate("templateIdTest1")
                                .idElement("elementIdTest1")
                                .unit("m")
                                .build(),
                        UnitVO.builder()
                                .idElement("elementIdTest2")
                                .unit("g")
                                .build(),
                        UnitVO.builder()
                                .idElement("elementIdTest2")
                                .unit("kg")
                                .build()));
        Collection<String> unitsAtElement1 = units.getUnits(null, "elementIdTest2");
        assertThat(unitsAtElement1.size(), equalTo(2));
        assertThat(unitsAtElement1, containsInAnyOrder("g", "kg"));
    }

    @Test(expectedExceptions = RuntimeException.class)
    public void should_find_empty_list_for_missing_element_id() {
        Units units = new Units();
        units.loadUnits(
                "testArchetype",
                null,
                Arrays.asList(
                        UnitVO.builder()
                                .idElement("elementIdTest1")
                                .unit("cm")
                                .build(),
                        UnitVO.builder()
                                .idTemplate("templateIdTest1")
                                .idElement("elementIdTest1")
                                .unit("cm")
                                .build()));
        units.getUnits(null, "elementIdTest2");
    }

    @Test(expectedExceptions = RuntimeException.class)
    public void should_find_throw_exception_for_missing_template_id() {
        Units units = new Units();
        units.loadUnits(
                "testArchetype",
                null,
                Arrays.asList(
                        UnitVO.builder()
                                .idElement("elementIdTest1")
                                .unit("cm")
                                .build(),
                        UnitVO.builder()
                                .idTemplate("templateIdTest1")
                                .idElement("elementIdTest1")
                                .unit("cm")
                                .build()));
        Collection<String> unitsAtElement1 = units.getUnits("templateIdTest2", "elementIdTest2");
        assertThat(unitsAtElement1, empty());
    }

    @Test
    public void should_find_units_in_after_second_load() {
        Units units = new Units();
        units.loadUnits(
                "testArchetype",
                null,
                Arrays.asList(
                        UnitVO.builder()
                                .idElement("testArchetype/elementIdTest1")
                                .unit("cm")
                                .build(),
                        UnitVO.builder()
                                .idElement("testArchetype/elementIdTest1")
                                .unit("m")
                                .build(),
                        UnitVO.builder()
                                .idElement("testArchetype/elementIdTest1")
                                .unit("in")
                                .build()));
        Collection<String> unitsAtElement1 = units.getUnits(null, "testArchetype/elementIdTest1");
        assertThat(unitsAtElement1.size(), equalTo(3));
        assertThat(unitsAtElement1, containsInAnyOrder("cm", "m", "in"));
        units.loadUnits(
                "testArchetype",
                null,
                Arrays.asList(
                        UnitVO.builder()
                                .idElement("testArchetype/elementIdTest1")
                                .unit("cm")
                                .build(),
                        UnitVO.builder()
                                .idElement("testArchetype/elementIdTest1")
                                .unit("m")
                                .build()));
        unitsAtElement1 = units.getUnits(null, "testArchetype/elementIdTest1");
        assertThat(unitsAtElement1.size(), equalTo(2));
        assertThat(unitsAtElement1, containsInAnyOrder("cm", "m"));
    }
}