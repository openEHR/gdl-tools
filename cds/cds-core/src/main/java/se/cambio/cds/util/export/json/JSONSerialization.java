package se.cambio.cds.util.export.json;

import se.cambio.openehr.util.exceptions.InternalErrorException;

/**
 * User: iago.corbal
 * Date: 2014-09-23
 * Time: 17:35
 */
public class JSONSerialization {
    private static JSONSerialization instance;

    private JSONSerialization(){

    }

    public static <E> String serialize(Class<E> clazz, E entity) throws InternalErrorException {
        return getSerializer(clazz).serialize(entity);
    }

    public static<E> E parse(Class<E> clazz, String src) throws InternalErrorException {
        return getSerializer(clazz).parse(src);
    }

    private static <E>JSONSerializer<E> getSerializer(Class<E> clazz){
        //Add custom serializer if needed
        return new DefaultSerializer<E>(clazz);
    }

    public static JSONSerialization getInstance() {
        if (instance == null) {
            instance = new JSONSerialization();
        }
        return instance;
    }
}
