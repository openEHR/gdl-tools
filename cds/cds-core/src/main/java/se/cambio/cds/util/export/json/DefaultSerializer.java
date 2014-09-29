package se.cambio.cds.util.export.json;

import com.google.gson.Gson;
import se.cambio.openehr.util.exceptions.InternalErrorException;

public class DefaultSerializer<E> implements JSONSerializer<E> {

    private Class<E> clazz;
    private Gson gson;

    public DefaultSerializer(Class<E> clazz) {
        this.clazz = clazz;
        gson = new Gson();
    }


    @Override
    public E parse(String src) throws InternalErrorException {
        return gson.fromJson(src, clazz);
    }

    @Override
    public String serialize(E entity) throws InternalErrorException {
        return gson.toJson(entity);
    }
}
