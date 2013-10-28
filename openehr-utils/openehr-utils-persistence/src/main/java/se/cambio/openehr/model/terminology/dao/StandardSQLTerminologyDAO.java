package se.cambio.openehr.model.terminology.dao;

import se.cambio.openehr.model.terminology.dto.TerminologyDTO;
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
public class StandardSQLTerminologyDAO implements SQLTerminologyDAO {


    /* (non-Javadoc)
     * @see es.sergas.canalejo.sisegtx.model.terminology.dao.SQLArquetipoDAO#buscar(java.sql.Connection, java.util.Collection)
     * Only loads the compiled terminology
     */
    public Collection<TerminologyDTO> searchByTerminologyIds(Connection connection, Collection<String> terminologyIds)
            throws InternalErrorException {

        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;
        Collection<TerminologyDTO> terminologyDTOs = new ArrayList<TerminologyDTO>();
        String terminologyIdStr = "";
        if (terminologyIds!=null && !terminologyIds.isEmpty()){
            for (String terminologyId : terminologyIds) {
                terminologyIdStr += terminologyId+",";
            }
            if (terminologyIdStr.length()>1){
                terminologyIdStr = terminologyIdStr.substring(0, terminologyIdStr.length()-1);
            }
        }else{
            return terminologyDTOs;
        }
        try {

	    /* Create "preparedStatement". */
            String queryString = "SELECT terminologyid, src"
                    + " FROM openehr_terminology WHERE terminologyid IN ("+terminologyIdStr+")";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Execute query. */
            resultSet = preparedStatement.executeQuery();

            if (!resultSet.next()) {
                return terminologyDTOs;
            }

	    /* Get results. */
            do {
                int i = 1;
                String terminologyId = resultSet.getString(i++);
                byte[] src = resultSet.getBytes(i++);
                terminologyDTOs.add(new TerminologyDTO(terminologyId, src));
            } while (resultSet.next());

	    /* Return the value object. */
            return terminologyDTOs;

        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }


    public Collection<TerminologyDTO> searchAll(Connection connection)
            throws InternalErrorException {
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;
        Collection<TerminologyDTO> terminologysVO = new ArrayList<TerminologyDTO>();
        try {
	    /* Create "preparedStatement". */
            String queryString =
                    "SELECT terminologyid, src FROM openehr_terminology";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Execute query. */
            resultSet = preparedStatement.executeQuery();

            if (!resultSet.next()) {
                return terminologysVO;
            }

	    /* Get results. */
            do {
                int i = 1;
                String terminologyId = resultSet.getString(i++);
                byte[] src = resultSet.getBytes(i++);
                terminologysVO.add(new TerminologyDTO(terminologyId, src));
            } while (resultSet.next());
	    /* Return the value object. */
            return terminologysVO;
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    public void insert(Connection connection, TerminologyDTO terminologyDTO)
            throws InternalErrorException {

        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;
        try {
	    /* Create "preparedStatement". */
            String queryString =
                    "INSERT INTO openehr_terminology (terminologyid, src) VALUES (?, ?)";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setString(i++, terminologyDTO.getTerminologyId());
            preparedStatement.setObject(i++, terminologyDTO.getSrc());
	    
	    /* Execute query. */
            int insertedRows = preparedStatement.executeUpdate();

            if (insertedRows == 0) {
                throw new SQLException("Cannot add record to 'openehr_terminology'");
            }
            if (insertedRows > 1) {
                throw new SQLException("Duplicate row at 'openehr_terminology'");
            }
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    public void update(Connection connection, TerminologyDTO terminologyDTO)
            throws InternalErrorException, InstanceNotFoundException {

        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;
        try {
	    /* Create "preparedStatement". */
            String queryString = "UPDATE openehr_terminology set"
                    + " src=? WHERE terminologyid=?";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setObject(i++, terminologyDTO.getSrc());
            preparedStatement.setString(i++, terminologyDTO.getTerminologyId());

	    /* Execute query. */
            int updatedRows = preparedStatement.executeUpdate();

            if (updatedRows == 0) {
                throw new InstanceNotFoundException(
                        terminologyDTO.getTerminologyId(),
                        TerminologyDTO.class.getName());
            }

            if (updatedRows > 1) {
                throw new SQLException("Duplicate row for terminology = '" +
                        terminologyDTO.getTerminologyId() + "' in table 'openehr_terminology'");
            }
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    public void remove(Connection connection, String terminologyId)
            throws InternalErrorException, InstanceNotFoundException{

        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;

        try {

	    /* Create "preparedStatement". */
            String queryString = "DELETE FROM openehr_terminology"
                    + " WHERE terminologyid = ?";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setString(i++, terminologyId);

	    /* Execute query. */
            int deletedRows = preparedStatement.executeUpdate();
            if (deletedRows == 0) {
                throw new InstanceNotFoundException(terminologyId,
                        TerminologyDTO.class.getName());
            }
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }
}