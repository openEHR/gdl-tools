package se.cambio.openehr.model.archetype.dao;

import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.sql.Connection;
import java.util.Collection;

/**
 * @author icorram
 *
 */
public interface SQLArchetypeDAO {
	/**
	 */
	public Collection<ArchetypeDTO> searchByArchetypeIds(Connection connection, Collection<String> archetypeIds)
	throws InternalErrorException;
	
	public Collection<ArchetypeDTO> searchAll(Connection connection)
	throws InternalErrorException;

    public Collection<ArchetypeDTO> searchAllDefinitions(Connection connection)
            throws InternalErrorException;
	
	public void insert(Connection connection, ArchetypeDTO ArchetypeDTO)
	throws InternalErrorException;
	
	public void update(Connection connection, ArchetypeDTO ArchetypeDTO)
	throws InternalErrorException, InstanceNotFoundException;
	
	public void remove(Connection conexion, String archetypeId)
	throws InternalErrorException, InstanceNotFoundException;
}
