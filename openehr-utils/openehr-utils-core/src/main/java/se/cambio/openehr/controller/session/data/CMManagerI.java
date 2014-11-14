package se.cambio.openehr.controller.session.data;

import se.cambio.cm.model.util.CMElement;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Collection;
import java.util.Date;

public interface CMManagerI<E extends CMElement>{
    public Collection<E> getAllInCache();
    public Collection<String> getAllIds() throws InternalErrorException;
    public Collection<String> getAllIdsInCache() throws InternalErrorException;
    public E getCMElement(final String id) throws InstanceNotFoundException, InternalErrorException;
    public Collection<E> getCMElementsInCache(final Collection<String> ids) throws InstanceNotFoundException, InternalErrorException;
    public E getCMElementInCache(final String id) throws InstanceNotFoundException, InternalErrorException;
    public Collection<E> getCMElementByIds(final Collection<String> ids) throws InstanceNotFoundException, InternalErrorException;
    public void remove(String id) throws InstanceNotFoundException, InternalErrorException;
    public void upsert(E cmElement) throws InternalErrorException;
    public Date getLastUpdate() throws InternalErrorException;
    public String getCachedChecksum() throws InternalErrorException;
}
