package se.cambio.cds.util;

import com.google.gson.Gson;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.util.export.CdsGsonBuilderFactory;

import java.util.ArrayList;
import java.util.Collection;

public class RepeatedArchetypeReferencesFilter {

    private Gson gson;

    public RepeatedArchetypeReferencesFilter() {
        this.gson = new CdsGsonBuilderFactory().getGsonBuilder().create();
    }

    public void filter(Collection<ArchetypeReference> archetypeReferences) {
        Collection<String> jsons = new ArrayList<>();
        Collection<ArchetypeReference> repeatedArchetypeReferences = new ArrayList<>();
        for (ArchetypeReference archetypeReference : archetypeReferences) {
            String json = gson.toJson(archetypeReference);
            if (jsons.contains(json)) {
                repeatedArchetypeReferences.add(archetypeReference);
            } else {
                jsons.add(json);
            }
        }
        archetypeReferences.removeAll(repeatedArchetypeReferences);
    }
}