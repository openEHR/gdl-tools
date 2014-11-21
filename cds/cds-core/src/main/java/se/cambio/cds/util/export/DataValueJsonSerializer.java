package se.cambio.cds.util.export;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;
import org.openehr.rm.datatypes.basic.DataValue;

import java.lang.reflect.Type;

public class DataValueJsonSerializer implements
        JsonSerializer<DataValue>, JsonDeserializer<DataValue> {

    @Override
    public DataValue deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
        return DataValue.parseValue(json.getAsString());
    }

    @Override
    public JsonElement serialize(DataValue src, Type typeOfSrc, JsonSerializationContext context) {
        return context.serialize(src.serialise());
    }
}
