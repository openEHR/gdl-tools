package se.cambio.cds.util.export.json;

import se.cambio.openehr.util.exceptions.InternalErrorException;

/**
 * User: iago.corbal
 * Date: 2014-09-23
 * Time: 19:25
 */
public interface JSONSerializer<E> {
    public E parse(String src) throws InternalErrorException;
    public String serialize(E entity) throws InternalErrorException;
}
