package se.cambio.cds.util.export;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;

import java.lang.reflect.Type;

public class ArchetypeReferenceJsonSerializer implements JsonDeserializer<ArchetypeReference> {

    private Gson gson;

    public ArchetypeReferenceJsonSerializer() {
        gson = new GsonBuilder()
                .registerTypeAdapter(DataValue.class, new DataValueJsonSerializer())
                .create();
    }

    @Override
    public ArchetypeReference deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
        ArchetypeReference archetypeReference = gson.fromJson(json, ArchetypeReference.class);
        for (ElementInstance elementInstance: archetypeReference.getElementInstancesMap().values()){
            elementInstance.setArchetypeReference(archetypeReference);
        }
        return archetypeReference;
    }
}
