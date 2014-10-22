package se.cambio.cm.model.cm.element.dao;

import se.cambio.cm.model.util.CMElement;
import se.cambio.openehr.util.exceptions.InternalErrorException;

public class CMElementBuilder<E extends CMElement> {

    public E build(Class<E> cmElementClass) throws InternalErrorException {
        try {
            return cmElementClass.newInstance();
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
    }
}
