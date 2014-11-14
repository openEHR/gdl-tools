package se.cambio.cm.model.cm.element.dao;

import se.cambio.cm.model.util.CMElement;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Collection;
import java.util.Date;

public interface GenericCMElementDAO<E extends CMElement> {

    public Collection<E> searchByIds(Collection<String> ids)
            throws InternalErrorException, InstanceNotFoundException;

    public Collection<E> searchAll() throws InternalErrorException;

    public Collection<String> searchAllIds() throws InternalErrorException;

    public void insert(E cmElement) throws InternalErrorException;

    public void update(E cmElement) throws InternalErrorException, InstanceNotFoundException;

    public void remove(String id)  throws InternalErrorException, InstanceNotFoundException;

    public void removeAll()  throws InternalErrorException;

    public Date getLastUpdateDate()  throws InternalErrorException;
}
