package se.cambio.cds.model.kb.instance.dao;

import se.cambio.cds.model.kb.instance.dto.KBInstanceDTO;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Collection;
import java.util.Date;

public interface GenericKBInstanceDAO {
    public Collection<KBInstanceDTO> searchByIds(Collection<String> ids) throws InternalErrorException, InstanceNotFoundException;
    public Collection<KBInstanceDTO> searchAll() throws InternalErrorException;
    public KBInstanceDTO upsert(KBInstanceDTO kbInstanceDTO) throws InternalErrorException;
    public void remove(String id) throws InternalErrorException, InstanceNotFoundException;
    public Date getLastUpdateDate() throws InternalErrorException;
}
