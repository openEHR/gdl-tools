package se.cambio.openehr.util;

import org.openehr.am.archetype.Archetype;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;

import java.util.*;

public class ArchetypeOnDemandMap extends AbstractMap<String, Archetype> {

    private Archetypes archetypes = null;

    public ArchetypeOnDemandMap(Archetypes archetypes) {
        this.archetypes = archetypes;
    }

    @Override
    public Set<Entry<String, Archetype>> entrySet() {
        Collection<Entry<String, Archetype>> entries = new ArrayList<>();
        for (Archetype archetype : archetypes.getArchetypeAOMsInCacheById(archetypes.getAllIdsInCache())) {
            ArchetypeEntry simpleEntry = new ArchetypeEntry(archetype);
            entries.add(simpleEntry);
        }
        return new HashSet<>(entries);
    }

    @Override
    public boolean containsKey(Object key) {
        if (archetypes.getAllIdsInCache().contains(key)) {
            return true;
        } else {
            try {
                archetypes.getCMElementByIds(Collections.singleton((String) key));
            } catch (InstanceNotFoundException ex) {
                return false;
            }
            return archetypes.getAllIdsInCache().contains(key);
        }
    }

    @Override
    public Archetype get(Object key) {
        try {
            Collection<Archetype> archetypesCollection = archetypes.getArchetypeAOMsByIds(Collections.singleton((String) key));
            if (archetypesCollection.isEmpty()) {
                return null;
            }
            return archetypesCollection.iterator().next();
        } catch (InstanceNotFoundException ex) {
            return null;
        }
    }

    private static class ArchetypeEntry implements Entry<String, Archetype> {
        private Archetype archetype;

        private ArchetypeEntry(Archetype archetype) {
            this.archetype = archetype;
        }

        @Override
        public String getKey() {
            return archetype.getArchetypeId().getValue();
        }

        @Override
        public Archetype getValue() {
            return archetype;
        }

        @Override
        public Archetype setValue(Archetype value) {
            this.archetype = value;
            return archetype;
        }
    }
}
