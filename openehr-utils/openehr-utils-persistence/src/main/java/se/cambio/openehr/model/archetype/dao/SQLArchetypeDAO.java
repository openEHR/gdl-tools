package se.cambio.openehr.model.archetype.dao;

import java.sql.Connection;
import java.util.Collection;

import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

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
	
	public void insert(Connection connection, ArchetypeDTO ArchetypeDTO)
	throws InternalErrorException;
	
	public void update(Connection connection, ArchetypeDTO ArchetypeDTO)
	throws InternalErrorException, InstanceNotFoundException;
	
	public void remove(Connection conexion, String archetypeId)
	throws InternalErrorException, InstanceNotFoundException;
}
