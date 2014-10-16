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
        Set<Entry<String, Archetype>> entries = new HashSet<Entry<String, Archetype>>();
        try {
            for (Archetype archetype : archetypes.getArchetypeAOMsInCacheById(archetypes.getAllIdsInCache())) {
                entries.add(new SimpleEntry<String, Archetype>(archetype.getArchetypeId().getValue(), archetype));
            }
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        } catch (InstanceNotFoundException e) {
            ExceptionHandler.handle(e);
        }
        return entries;
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
                Collection<Archetype> archetypesCollection = archetypes.getArchetypeAOMsById(Collections.singleton((String)key));
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
}
