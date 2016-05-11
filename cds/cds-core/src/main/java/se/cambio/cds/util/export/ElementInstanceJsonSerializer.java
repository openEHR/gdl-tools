package se.cambio.cds.util.export;

import com.google.gson.*;
import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.cds.model.facade.execution.vo.GeneratedElementInstance;
import se.cambio.cds.model.instance.ElementInstance;

import java.lang.reflect.Type;

public class ElementInstanceJsonSerializer implements JsonDeserializer<ElementInstance> {

    private Gson gson;

    public ElementInstanceJsonSerializer() {
        gson = new GsonBuilder()
                .registerTypeAdapter(DataValue.class, new DataValueJsonSerializer())
                .create();
    }

    @Override
    public ElementInstance deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
        JsonElement ruleReferences = json.getAsJsonObject().get("ruleReferences");
        if (ruleReferences != null) {
            return gson.fromJson(json, GeneratedElementInstance.class);
        } else {
            return gson.fromJson(json, ElementInstance.class);
        }
    }
}
