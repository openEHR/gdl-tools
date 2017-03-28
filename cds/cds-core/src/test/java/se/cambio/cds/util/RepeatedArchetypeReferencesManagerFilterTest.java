package se.cambio.cds.util;

import org.junit.Test;
import org.openehr.rm.datatypes.basic.DvBoolean;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;

import java.util.ArrayList;
import java.util.Collection;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

public class RepeatedArchetypeReferencesManagerFilterTest {
    @Test
    public void should_detect_repeated_archetype_references() {
        ArchetypeReference ar1 = new ArchetypeReference("EHR", "testArchetype", null);
        ArchetypeReference ar2 = new ArchetypeReference("EHR", "testArchetype", null);
        new ElementInstance("testPath1", new DvBoolean(true), ar1, null, null);
        new ElementInstance("testPath1", new DvBoolean(true), ar2, null, null);
        Collection<ArchetypeReference> archetypeReferences = new ArrayList<>();
        archetypeReferences.add(ar1);
        archetypeReferences.add(ar2);

        RepeatedArchetypeReferencesFilter repeatedArchetypeReferencesFilter = new RepeatedArchetypeReferencesFilter();
        repeatedArchetypeReferencesFilter.filter(archetypeReferences);
        assertThat(archetypeReferences.size(), equalTo(1));
    }

    @Test
    public void should_not_detect_repeated_archetype_references_with_differente_data_value() {
        ArchetypeReference ar1 = new ArchetypeReference("EHR", "testArchetype", null);
        ArchetypeReference ar2 = new ArchetypeReference("EHR", "testArchetype", null);
        new ElementInstance("testPath1", new DvBoolean(true), ar1, null, null);
        new ElementInstance("testPath1", new DvBoolean(false), ar2, null, null);
        Collection<ArchetypeReference> archetypeReferences = new ArrayList<>();
        archetypeReferences.add(ar1);
        archetypeReferences.add(ar2);

        RepeatedArchetypeReferencesFilter repeatedArchetypeReferencesFilter = new RepeatedArchetypeReferencesFilter();
        repeatedArchetypeReferencesFilter.filter(archetypeReferences);
        assertThat(archetypeReferences.size(), equalTo(2));
    }

    @Test
    public void should_not_detect_repeated_archetype_references_with_different_template() {
        ArchetypeReference ar1 = new ArchetypeReference("EHR", "testArchetype", null);
        ArchetypeReference ar2 = new ArchetypeReference("EHR", "testArchetype", "testTemplate1");
        new ElementInstance("testPath1", new DvBoolean(true), ar1, null, null);
        new ElementInstance("testPath1", new DvBoolean(true), ar2, null, null);
        Collection<ArchetypeReference> archetypeReferences = new ArrayList<>();
        archetypeReferences.add(ar1);
        archetypeReferences.add(ar2);

        RepeatedArchetypeReferencesFilter repeatedArchetypeReferencesFilter = new RepeatedArchetypeReferencesFilter();
        repeatedArchetypeReferencesFilter.filter(archetypeReferences);
        assertThat(archetypeReferences.size(), equalTo(2));
    }

    @Test
    public void should_not_detect_repeated_archetype_references_with_different_path() {
        ArchetypeReference ar1 = new ArchetypeReference("EHR", "testArchetype", null);
        ArchetypeReference ar2 = new ArchetypeReference("EHR", "testArchetype", null);
        new ElementInstance("testPath1", new DvBoolean(true), ar1, null, null);
        new ElementInstance("testPath2", new DvBoolean(true), ar2, null, null);
        Collection<ArchetypeReference> archetypeReferences = new ArrayList<>();
        archetypeReferences.add(ar1);
        archetypeReferences.add(ar2);

        RepeatedArchetypeReferencesFilter repeatedArchetypeReferencesFilter = new RepeatedArchetypeReferencesFilter();
        repeatedArchetypeReferencesFilter.filter(archetypeReferences);
        assertThat(archetypeReferences.size(), equalTo(2));
    }
}
