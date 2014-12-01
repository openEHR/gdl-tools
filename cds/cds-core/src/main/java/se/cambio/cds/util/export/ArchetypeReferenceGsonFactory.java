package se.cambio.cds.util.export;

import com.google.gson.ExclusionStrategy;
import com.google.gson.FieldAttributes;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.cds.model.instance.ArchetypeReference;

public class ArchetypeReferenceGsonFactory {
    public ArchetypeReferenceGsonFactory() {
    }

    public Gson create(){
        return new GsonBuilder()
                .setPrettyPrinting()
                .registerTypeAdapter(DataValue.class, new DataValueJsonSerializer())
                .registerTypeAdapter(ArchetypeReference.class, new ArchetypeReferenceJsonSerializer())
                .setExclusionStrategies(new ExclusionStrategy() {
                    @Override
                    public boolean shouldSkipField(FieldAttributes f) {
                        return f.getName().equals("archetypeReference");
                    }

                    @Override
                    public boolean shouldSkipClass(Class<?> clazz) {
                        return false;
                    }
                })
                .create();
    }
}
