/*
 * Created on 02-jun-2006
 *


 */
package se.cambio.openehr.model.template.dao;

import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.model.util.OpenEHRGlobalNames;
import se.cambio.openehr.model.util.sql.DataSourceLocator;
import se.cambio.openehr.model.util.sql.GeneralOperations;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Collection;
/**
 * @author icorram
 *


 */
public class SQLGenericTemplateDAO implements GenericTemplateDAO {

	private SQLTemplateDAO dao;
	private DataSource dataSource;
	private DataSource dataSourceRR;

	public SQLGenericTemplateDAO() throws InternalErrorException {

		dao = SQLTemplateFactory.getDAO();
		dataSource = DataSourceLocator.getDataSource(OpenEHRGlobalNames.OPENEHR_DATA_SOURCE);
        //TODO Get proper RepetableRead DS
        dataSourceRR = DataSourceLocator.getDataSource(OpenEHRGlobalNames.OPENEHR_DATA_SOURCE);
	}
	/* (non-Javadoc)
	 * @see es.sergas.canalejo.sisegtx.model.diagnostico.dao.GenericDiagnosticoDAO#buscar(java.sql.Connection, java.lang.Short)
	 */
	public Collection<TemplateDTO> searchByTemplateIds(Collection<String> templateIds) 
	throws InternalErrorException {
		Connection connection = null;
		try {
			connection = dataSource.getConnection();
			return dao.searchByTemplateIds(connection, templateIds);
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeConnection(connection);
		}
	}

	public Collection<TemplateDTO> searchAll() 
	throws InternalErrorException {
		Connection connection = null;
		try {
			connection = dataSource.getConnection();
			return dao.searchAll(connection);
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeConnection(connection);
		}
	}

    @Override
    public Collection<TemplateDTO> searchAllDefinitions() throws InternalErrorException {
        Connection connection = null;
        try {
            connection = dataSource.getConnection();
            return dao.searchAllDefinitions(connection);
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeConnection(connection);
        }
    }


    public void insert(TemplateDTO templateDTO)
	throws InternalErrorException {
		Connection connection = null;
		try {
			connection = dataSourceRR.getConnection();
			dao.insert(connection, templateDTO);
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeConnection(connection);
		}
	}

	public void update(TemplateDTO templateDTO) 
	throws InternalErrorException, InstanceNotFoundException {
		Connection connection = null;
		try {
			connection = dataSourceRR.getConnection();
			dao.update(connection, templateDTO);
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeConnection(connection);
		}
	}
	
	public void remove(String templateId) 
	throws InternalErrorException, InstanceNotFoundException {
		Connection connection = null;
		try {
			connection = dataSourceRR.getConnection();
			dao.remove(connection, templateId);
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeConnection(connection);
		}
	}
}
