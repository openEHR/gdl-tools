package se.cambio.openehr.model.template.dao;

import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.sql.Connection;
import java.util.Collection;

/**
 * @author icorram
 *
 */
public interface SQLTemplateDAO {
	/**
	 */
	public Collection<TemplateDTO> searchByTemplateIds(Connection connection, Collection<String> idsTemplates)
	throws InternalErrorException;
	
	public Collection<TemplateDTO> searchAll(Connection connection)
	throws InternalErrorException;

    public Collection<TemplateDTO> searchAllDefinitions(Connection connection)
            throws InternalErrorException;

	public void insert(Connection connection, TemplateDTO templateDTO)
	throws InternalErrorException;
	
	public void update(Connection connection, TemplateDTO templateDTO)
	throws InternalErrorException, InstanceNotFoundException;
	
	public void remove(Connection connection, String templateId)
	throws InternalErrorException, InstanceNotFoundException;
}
