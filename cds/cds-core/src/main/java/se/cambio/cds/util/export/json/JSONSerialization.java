package se.cambio.cds.util.export.json;

import se.cambio.cds.model.kb.instance.KBInstance;
import se.cambio.openehr.util.exceptions.InternalErrorException;


public class JSONSerialization {
    private static JSONSerialization instance;

    private JSONSerialization(){

    }

    public static <E> String serialize(Class<E> clazz, E entity) throws InternalErrorException {
        return getSerializer(clazz).serialize(entity);
    }

    public static <E> E parse(Class<E> clazz, String src) throws InternalErrorException {
        return (E)getSerializer(clazz).parse(src);
    }

    private static JSONSerializer getSerializer(Class clazz){
        if (KBInstance.class.isAssignableFrom(clazz)){
            return new KBInstanceSerializer();
        } else {
            return new DefaultSerializer(clazz);
        }
    }

    public static JSONSerialization getInstance() {
        if (instance == null) {
            instance = new JSONSerialization();
        }
        return instance;
    }
}
