package se.cambio.openehr.util;

import org.openehr.am.archetype.Archetype;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;

public class ArchetypeOnDemandMap extends AbstractMap<String, Archetype> {

    private Archetypes archetypes = null;

    public ArchetypeOnDemandMap(Archetypes archetypes) {
        this.archetypes = archetypes;
    }

    @Override
    public Set<Entry<String, Archetype>> entrySet() {
        Collection<Entry<String, Archetype>> entries = new ArrayList<>();
        try {
            for (Archetype archetype : archetypes.getArchetypeAOMsInCacheById(archetypes.getAllIdsInCache())) {
                ArchetypeEntry simpleEntry = new ArchetypeEntry(archetype);
                entries.add(simpleEntry);
            }
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        } catch (InstanceNotFoundException e) {
            ExceptionHandler.handle(e);
        }
        return new HashSet<>(entries);
    }

    @Override
    public boolean containsKey(Object key) {
        try {
            if (archetypes.getAllIdsInCache().contains(key)) {
                return true;
            } else {
                try {
                    archetypes.getCMElementByIds(Collections.singleton((String)key));
                } catch (InstanceNotFoundException e) {
                    return false;
                }
                return archetypes.getAllIdsInCache().contains(key);
            }
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        }
        return false;
    }

    @Override
    public Archetype get(Object key) {
        try {
            try {
                Collection<Archetype> archetypesCollection = archetypes.getArchetypeAOMsByIds(Collections.singleton((String) key));
                if (archetypesCollection.isEmpty()){
                    return null;
                }
                return archetypesCollection.iterator().next();
            } catch (InstanceNotFoundException e) {
                return null;
            }
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        }
        return null;
    }

    private static class ArchetypeEntry implements Entry<String, Archetype>{
        private Archetype archetype;

        private ArchetypeEntry(Archetype archetype){
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
