package se.cambio.cds.model.cm.element.dao;

import se.cambio.openehr.model.util.CMElement;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.lang.reflect.ParameterizedType;

public class CMElementBuilder<E extends CMElement> {

    public E build(String id) throws InternalErrorException {
        try {
            return getCMElementClass().getConstructor(String.class).newInstance(id);
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
    }

    private Class<E> getCMElementClass() {
        return (Class<E>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[0];
    }
}
