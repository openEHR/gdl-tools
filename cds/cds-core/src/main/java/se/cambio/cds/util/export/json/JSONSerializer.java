package se.cambio.cds.util.export.json;

import se.cambio.openehr.util.exceptions.InternalErrorException;

public interface JSONSerializer<E> {
    public E parse(String src) throws InternalErrorException;
    public String serialize(E entity) throws InternalErrorException;
}
