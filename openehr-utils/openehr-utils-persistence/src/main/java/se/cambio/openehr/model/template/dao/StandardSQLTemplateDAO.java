package se.cambio.openehr.model.template.dao;

import se.cambio.openehr.model.template.dto.TemplateDTO;
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
public class StandardSQLTemplateDAO implements SQLTemplateDAO {


    /* (non-Javadoc)
     * @see es.sergas.canalejo.sisegtx.model.Template.dao.SQLTemplateDAO#buscar(java.sql.Connection, java.util.Collection)
     * Only loads the compiled archetype
     */
    public Collection<TemplateDTO> searchByTemplateIds(Connection connection, Collection<String> templateIds)
            throws InternalErrorException {

        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;
        Collection<TemplateDTO> templateDTO = new ArrayList<TemplateDTO>();
        String templateIdsStr = "";
        if (templateIds!=null && !templateIds.isEmpty()){
            for (String idTemplate : templateIds) {
                templateIdsStr += "'"+idTemplate+"',";
            }
            if (templateIdsStr.length()>1){
                templateIdsStr = templateIdsStr.substring(0, templateIdsStr.length()-1);
            }
        }else{
            return templateDTO;
        }
        try {

	    /* Create "preparedStatement". */
            String queryString = "SELECT templateid, archetypeid, name, description, rmname, archetype, aom, tobcvo"
                    + " FROM openehr_template WHERE templateid IN ("+templateIdsStr+")";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Execute query. */
            resultSet = preparedStatement.executeQuery();

            if (!resultSet.next()) {
                return templateDTO;
            }

	    /* Get results. */
            do {
                int i = 1;
                String templateId = resultSet.getString(i++);
                String idArchetype = resultSet.getString(i++);
                String name = resultSet.getString(i++);
                String description = resultSet.getString(i++);
                String rmName = resultSet.getString(i++);
                String archetype = resultSet.getString(i++);
                byte[] aom = resultSet.getBytes(i++);
                byte[] tobcvo = resultSet.getBytes(i++);
                templateDTO.add(new TemplateDTO(templateId, idArchetype, name, description, rmName, archetype, aom, tobcvo));
            } while (resultSet.next());

	    /* Return the value object. */
            return templateDTO;

        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }


    public Collection<TemplateDTO> searchAll(Connection connection)
            throws InternalErrorException {
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;
        Collection<TemplateDTO> templateDTOs = new ArrayList<TemplateDTO>();
        try {
	    /* Create "preparedStatement". */
            String queryString =
                    "SELECT templateid, archetypeid, name, description, rmname, archetype, aom, tobcvo FROM openehr_template";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Execute query. */
            resultSet = preparedStatement.executeQuery();

            if (!resultSet.next()) {
                return templateDTOs;
            }

	    /* Get results. */
            do {
                int i = 1;
                String idTemplate = resultSet.getString(i++);
                String idArchetype = resultSet.getString(i++);
                String name = resultSet.getString(i++);
                String description = resultSet.getString(i++);
                String rmName = resultSet.getString(i++);
                String archetype = resultSet.getString(i++);
                byte[] aom = resultSet.getBytes(i++);
                byte[] tobcvo = resultSet.getBytes(i++);
                templateDTOs.add(new TemplateDTO(idTemplate, name, description, idArchetype, rmName, archetype, aom, tobcvo));
            } while (resultSet.next());

	    /* Return the value object. */
            return templateDTOs;

        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    public Collection<TemplateDTO> searchAllDefinitions(Connection connection)
            throws InternalErrorException {
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;
        Collection<TemplateDTO> templateDTOs = new ArrayList<TemplateDTO>();
        try {
	    /* Create "preparedStatement". */
            String queryString =
                    "SELECT templateid, archetypeid, rmname, archetype FROM openehr_template";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Execute query. */
            resultSet = preparedStatement.executeQuery();

            if (!resultSet.next()) {
                return templateDTOs;
            }

	    /* Get results. */
            do {
                int i = 1;
                String idTemplate = resultSet.getString(i++);
                String idArchetype = resultSet.getString(i++);
                String name = resultSet.getString(i++);
                String description = resultSet.getString(i++);
                String rmName = resultSet.getString(i++);
                String archetype = resultSet.getString(i++);
                templateDTOs.add(new TemplateDTO(idTemplate, idArchetype, name, description, rmName, archetype, null, null));
            } while (resultSet.next());

	    /* Return the value object. */
            return templateDTOs;

        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    public void insert(Connection connection, TemplateDTO templateDTO)
            throws InternalErrorException {

        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;
        try {
	    /* Create "preparedStatement". */
            String queryString = "INSERT INTO openehr_template (templateid, archetypeid, name, description, rmname, archetype, aom, tobcvo)"
                    + " VALUES (?, ?, ?, ?, ?, ?, ?, ?)";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setString(i++, templateDTO.getIdTemplate());
            preparedStatement.setString(i++, templateDTO.getIdArchetype());
            preparedStatement.setString(i++, templateDTO.getName());
            preparedStatement.setString(i++, templateDTO.getDescription());
            preparedStatement.setString(i++, templateDTO.getRMName());
            preparedStatement.setString(i++, templateDTO.getArchetype());
            preparedStatement.setObject(i++, templateDTO.getAom());
            preparedStatement.setObject(i++, templateDTO.getTobcVO());

	    /* Execute query. */
            int insertedRows = preparedStatement.executeUpdate();

            if (insertedRows == 0) {
                throw new SQLException("Cannot add row to 'openehr_template'");
            }
            if (insertedRows > 1) {
                throw new SQLException("Duplicate row at 'openehr_template'");
            }
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    public void update(Connection connection, TemplateDTO templateDTO)
            throws InternalErrorException, InstanceNotFoundException {

        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;
        try {
	    /* Create "preparedStatement". */
            String queryString = "UPDATE openehr_template set"
                    + " name=?, description=?, archetype=?, aom=?, tobcvo=? WHERE templateid=?";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setString(i++, templateDTO.getName());
            preparedStatement.setString(i++, templateDTO.getDescription());
            preparedStatement.setString(i++, templateDTO.getArchetype());
            preparedStatement.setObject(i++, templateDTO.getAom());
            preparedStatement.setObject(i++, templateDTO.getTobcVO());
            preparedStatement.setString(i++, templateDTO.getIdTemplate());

	    /* Execute query. */
            int updatedRows = preparedStatement.executeUpdate();

            if (updatedRows == 0) {
                throw new InstanceNotFoundException(
                        templateDTO.getIdTemplate(),
                        TemplateDTO.class.getName());
            }

            if (updatedRows > 1) {
                throw new SQLException("Duplicate row for template = '" +
                        templateDTO.getIdTemplate() + "' in table 'openehr_template'");
            }
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    public void remove(Connection connection, String templateId)
            throws InternalErrorException, InstanceNotFoundException{

        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;

        try {

	    /* Create "preparedStatement". */
            String queryString = "DELETE FROM openehr_template"
                    + " WHERE templateid = ?";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setString(i++, templateId);

	    /* Execute query. */
            int removedRows = preparedStatement.executeUpdate();
            if (removedRows == 0) {
                throw new InstanceNotFoundException(templateId,
                        TemplateDTO.class.getName());
            }
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }
}