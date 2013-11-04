/*
 * Created on 02-jun-2006
 *


 */
package se.cambio.openehr.model.terminology.dao;

import se.cambio.openehr.model.terminology.dto.TerminologyDTO;
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
public class SQLGenericTerminologyDAO implements GenericTerminologyDAO {

    private SQLTerminologyDAO dao;
    private DataSource dataSource;
    private DataSource dataSourceRR;

    public SQLGenericTerminologyDAO() throws InternalErrorException {

        dao = SQLTerminologyFactory.getDAO();
        dataSource = DataSourceLocator.getDataSource(OpenEHRGlobalNames.OPENEHR_DATA_SOURCE);
        //TODO Get proper RepetableRead DS
        dataSourceRR = DataSourceLocator.getDataSource(OpenEHRGlobalNames.OPENEHR_DATA_SOURCE);
    }

    public Collection<TerminologyDTO> searchByTerminologyIds(Collection<String> terminologyIds)
            throws InternalErrorException {
        Connection connection = null;
        try {
            connection = dataSource.getConnection();
            return dao.searchByTerminologyIds(connection, terminologyIds);
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeConnection(connection);
        }
    }

    public Collection<TerminologyDTO> searchAll()
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


    public void insert(TerminologyDTO TerminologyDTO)
            throws InternalErrorException {
        Connection connection = null;
        try {
            connection = dataSourceRR.getConnection();
            dao.insert(connection, TerminologyDTO);
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeConnection(connection);
        }
    }

    public void update(TerminologyDTO TerminologyDTO)
            throws InternalErrorException, InstanceNotFoundException {
        Connection connection = null;
        try {
            connection = dataSourceRR.getConnection();
            dao.update(connection, TerminologyDTO);
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeConnection(connection);
        }
    }

    public void remove(String terminologyId)
            throws InternalErrorException, InstanceNotFoundException {
        Connection connection = null;
        try {
            connection = dataSourceRR.getConnection();
            dao.remove(connection, terminologyId);
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeConnection(connection);
        }
    }
}
