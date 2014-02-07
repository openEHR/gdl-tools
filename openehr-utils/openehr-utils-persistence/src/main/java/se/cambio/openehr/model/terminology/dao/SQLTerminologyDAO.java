package se.cambio.openehr.model.terminology.dao;

import se.cambio.openehr.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.sql.Connection;
import java.util.Collection;

/**
 * @author icorram
 *
 */
public interface SQLTerminologyDAO {
    /**
     */
    public Collection<TerminologyDTO> searchByTerminologyIds(Connection connection, Collection<String> terminologyIds)
            throws InternalErrorException;

    public Collection<TerminologyDTO> searchAll(Connection connection)
            throws InternalErrorException;

    public void insert(Connection connection, TerminologyDTO TerminologyDTO)
            throws InternalErrorException;

    public void update(Connection connection, TerminologyDTO TerminologyDTO)
            throws InternalErrorException, InstanceNotFoundException;

    public void remove(Connection conexion, String terminologyId)
            throws InternalErrorException, InstanceNotFoundException;
}
