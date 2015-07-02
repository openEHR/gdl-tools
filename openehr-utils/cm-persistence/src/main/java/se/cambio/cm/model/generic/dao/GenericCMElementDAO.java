package se.cambio.cm.model.generic.dao;

import se.cambio.cm.model.util.CMElement;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Collection;
import java.util.Date;

public interface GenericCMElementDAO<E extends CMElement> {

    Collection<E> searchByIds(Collection<String> ids)
            throws InternalErrorException, InstanceNotFoundException;

    Collection<E> searchAll() throws InternalErrorException;

    Collection<String> searchAllIds() throws InternalErrorException;

    void insert(E cmElement) throws InternalErrorException;

    void update(E cmElement) throws InternalErrorException, InstanceNotFoundException;

    void remove(String id)  throws InternalErrorException, InstanceNotFoundException;

    void removeAll()  throws InternalErrorException;

    Date getLastUpdateDate()  throws InternalErrorException;
}
