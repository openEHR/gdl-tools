/*
 * Created on 02-jun-2006
 *


 */
package se.cambio.openehr.model.archetype.dao;

import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
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
public class SQLGenericArchetypeDAO implements GenericArchetypeDAO {

    private SQLArchetypeDAO dao;
    private DataSource dataSource;
    private DataSource dataSourceRR;

    public SQLGenericArchetypeDAO() throws InternalErrorException {

        dao = SQLArchetypeFactory.getDAO();
        dataSource = DataSourceLocator.getDataSource(OpenEHRGlobalNames.OPENEHR_DATA_SOURCE);
        //TODO Get proper RepetableRead DS
        dataSourceRR = DataSourceLocator.getDataSource(OpenEHRGlobalNames.OPENEHR_DATA_SOURCE);
    }
    /* (non-Javadoc)
     * @see es.sergas.canalejo.sisegtx.model.diagnostico.dao.GenericDiagnosticoDAO#buscar(java.sql.Connection, java.lang.Short)
     */
    public Collection<ArchetypeDTO> searchByArchetypeIds(Collection<String> archetypeIds)
            throws InternalErrorException {
        Connection connection = null;
        try {
            connection = dataSource.getConnection();
            return dao.searchByArchetypeIds(connection, archetypeIds);
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeConnection(connection);
        }
    }

    @Override
    public Collection<ArchetypeDTO> searchAllDefinitions() throws InternalErrorException {
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

    public Collection<ArchetypeDTO> searchAll()
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


    public void insert(ArchetypeDTO ArchetypeDTO)
            throws InternalErrorException {
        Connection connection = null;
        try {
            connection = dataSourceRR.getConnection();
            dao.insert(connection, ArchetypeDTO);
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeConnection(connection);
        }
    }

    public void update(ArchetypeDTO ArchetypeDTO)
            throws InternalErrorException, InstanceNotFoundException {
        Connection connection = null;
        try {
            connection = dataSourceRR.getConnection();
            dao.update(connection, ArchetypeDTO);
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeConnection(connection);
        }
    }

    public void remove(String archetypeId)
            throws InternalErrorException, InstanceNotFoundException {
        Connection connection = null;
        try {
            connection = dataSourceRR.getConnection();
            dao.remove(connection, archetypeId);
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeConnection(connection);
        }
    }
}
