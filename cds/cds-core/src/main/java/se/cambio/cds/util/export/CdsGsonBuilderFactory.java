package se.cambio.cds.util.export;

import com.google.gson.ExclusionStrategy;
import com.google.gson.FieldAttributes;
import com.google.gson.GsonBuilder;
import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.cds.model.instance.ArchetypeReference;

public class CdsGsonBuilderFactory {
    public CdsGsonBuilderFactory() {
    }

    public GsonBuilder getGsonBuilder() {
        return new GsonBuilder()
                .registerTypeAdapter(DataValue.class, new DataValueJsonSerializer())
                .registerTypeAdapter(ArchetypeReference.class, new ArchetypeReferenceJsonSerializer())
                .registerTypeHierarchyAdapter(byte[].class, new ByteArrayToBase64TypeAdapter())
                .setDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
                .setExclusionStrategies(new ExclusionStrategy() {
                    @Override
                    public boolean shouldSkipField(FieldAttributes reference) {
                        return reference.getName().equals("archetypeReference");
                    }

                    @Override
                    public boolean shouldSkipClass(Class<?> clazz) {
                        return false;
                    }
                });
    }
}
