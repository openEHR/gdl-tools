package se.cambio.openehr.model.archetype.dao;

import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.util.sql.GeneralOperations;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;

/**
 * @author icorram
 *
 *
 */
public class StandardSQLArchetypeDAO implements SQLArchetypeDAO {

    @Override
    public Collection<ArchetypeDTO> searchAllDefinitions(Connection connection) throws InternalErrorException {
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;
        Collection<ArchetypeDTO> archetypesVO = new ArrayList<ArchetypeDTO>();
        try {
	    /* Create "preparedStatement". */
            String queryString =
                    "SELECT archetypeid, rmname, archetype FROM openehr_archetype";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Execute query. */
            resultSet = preparedStatement.executeQuery();

            if (!resultSet.next()) {
                return archetypesVO;
            }

	    /* Get results. */
            do {
                int i = 1;
                String archetypeId = resultSet.getString(i++);
                String rmName = resultSet.getString(i++);
                String archetype = resultSet.getString(i++);
                archetypesVO.add(new ArchetypeDTO(archetypeId, archetypeId, archetypeId, rmName, archetype, null, null));
            } while (resultSet.next());
	    /* Return the value object. */
            return archetypesVO;
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    /* (non-Javadoc)
         * @see es.sergas.canalejo.sisegtx.model.archetype.dao.SQLArquetipoDAO#buscar(java.sql.Connection, java.util.Collection)
         * Only loads the compiled archetype
         */
    public Collection<ArchetypeDTO> searchByArchetypeIds(Connection connection, Collection<String> archetypeIds)
            throws InternalErrorException {

        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;
        Collection<ArchetypeDTO> archetypesVO = new ArrayList<ArchetypeDTO>();
        String idsArquetiposStr = "";
        if (archetypeIds!=null && !archetypeIds.isEmpty()){
            for (String archetypeId : archetypeIds) {
                idsArquetiposStr += archetypeId+",";
            }
            if (idsArquetiposStr.length()>1){
                idsArquetiposStr = idsArquetiposStr.substring(0, idsArquetiposStr.length()-1);
            }
        }else{
            return archetypesVO;
        }
        try {

	    /* Create "preparedStatement". */
            String queryString = "SELECT archetypeid, rmname, archetype, aom, aobcvo"
                    + " FROM openehr_archetype WHERE archetypeid IN ("+idsArquetiposStr+")";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Execute query. */
            resultSet = preparedStatement.executeQuery();

            if (!resultSet.next()) {
                return archetypesVO;
            }

	    /* Get results. */
            do {
                int i = 1;
                String archetypeId = resultSet.getString(i++);
                String rmName = resultSet.getString(i++);
                String archetype = resultSet.getString(i++);
                byte[] aom = resultSet.getBytes(i++);
                byte[] aobcvo = resultSet.getBytes(i++);
                archetypesVO.add(new ArchetypeDTO(archetypeId, archetypeId , archetypeId, rmName, archetype, aom, aobcvo));
            } while (resultSet.next());

	    /* Return the value object. */
            return archetypesVO;

        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }


    public Collection<ArchetypeDTO> searchAll(Connection connection)
            throws InternalErrorException {
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;
        Collection<ArchetypeDTO> archetypesVO = new ArrayList<ArchetypeDTO>();
        try {
	    /* Create "preparedStatement". */
            String queryString =
                    "SELECT archetypeid, rmname, archetype, aom, aobcvo FROM openehr_archetype";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Execute query. */
            resultSet = preparedStatement.executeQuery();

            if (!resultSet.next()) {
                return archetypesVO;
            }

	    /* Get results. */
            do {
                int i = 1;
                String archetypeId = resultSet.getString(i++);
                String rmName = resultSet.getString(i++);
                String archetype = resultSet.getString(i++);
                byte[] aom = resultSet.getBytes(i++);
                byte[] aobcvo = resultSet.getBytes(i++);
                archetypesVO.add(new ArchetypeDTO(archetypeId, archetypeId, archetypeId, rmName, archetype, aom, aobcvo));
            } while (resultSet.next());
	    /* Return the value object. */
            return archetypesVO;
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    public void insert(Connection connection, ArchetypeDTO archetypeDTO)
            throws InternalErrorException {

        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;
        try {
	    /* Create "preparedStatement". */
            String queryString =
                    "INSERT INTO openehr_archetype (archetypeid, rmname, archetype, aom, aobcvo) VALUES (?, ?, ?, ?, ?)";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setString(i++, archetypeDTO.getIdArchetype());
            preparedStatement.setString(i++, archetypeDTO.getRMName());
            preparedStatement.setString(i++, archetypeDTO.getArchetype());
            preparedStatement.setObject(i++, archetypeDTO.getAom());
            preparedStatement.setObject(i++, archetypeDTO.getAobcVO());
	    
	    /* Execute query. */
            int insertedRows = preparedStatement.executeUpdate();

            if (insertedRows == 0) {
                throw new SQLException("Cannot add record to 'openehr_archetype'");
            }
            if (insertedRows > 1) {
                throw new SQLException("Duplicate row at 'openehr_archetype'");
            }
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    public void update(Connection connection, ArchetypeDTO archetypeDTO)
            throws InternalErrorException, InstanceNotFoundException {

        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;
        try {
	    /* Create "preparedStatement". */
            String queryString = "UPDATE openehr_archetype set"
                    + " archetype=?, aom=?, aobcvo=? WHERE archetypeid=?";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setString(i++, archetypeDTO.getArchetype());
            preparedStatement.setObject(i++, archetypeDTO.getAom());
            preparedStatement.setObject(i++, archetypeDTO.getAobcVO());
            preparedStatement.setString(i++, archetypeDTO.getIdArchetype());

	    /* Execute query. */
            int updatedRows = preparedStatement.executeUpdate();

            if (updatedRows == 0) {
                throw new InstanceNotFoundException(
                        archetypeDTO.getIdArchetype(),
                        ArchetypeDTO.class.getName());
            }

            if (updatedRows > 1) {
                throw new SQLException("Duplicate row for archetype = '" +
                        archetypeDTO.getIdArchetype() + "' in table 'openehr_archetype'");
            }
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    public void remove(Connection connection, String archetypeId)
            throws InternalErrorException, InstanceNotFoundException{

        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;

        try {

	    /* Create "preparedStatement". */
            String queryString = "DELETE FROM openehr_archetype"
                    + " WHERE archetypeid = ?";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setString(i++, archetypeId);

	    /* Execute query. */
            int deletedRows = preparedStatement.executeUpdate();
            if (deletedRows == 0) {
                throw new InstanceNotFoundException(archetypeId,
                        ArchetypeDTO.class.getName());
            }
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }
}