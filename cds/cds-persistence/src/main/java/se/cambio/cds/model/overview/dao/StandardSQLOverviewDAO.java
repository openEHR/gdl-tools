package se.cambio.cds.model.overview.dao;

import se.cambio.cds.model.overview.dto.OverviewDTO;
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
 * @author iago.corbal
 *
 */
public class StandardSQLOverviewDAO implements SQLOverviewDAO {

    public OverviewDTO searchByOverviewId(Connection connection, String overviewId)
            throws InternalErrorException, InstanceNotFoundException {
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;

        try {
	    /* Create "preparedStatement". */
            String queryString = "SELECT name, description, src FROM cds_overview WHERE overviewid = ?";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setString(i++, overviewId);

	    /* Execute query. */
            resultSet = preparedStatement.executeQuery();

            if (!resultSet.next()) {
                throw new InstanceNotFoundException(overviewId, OverviewDTO.class.getName());
            }

	    /* Get results. */
            i = 1;
            String name = resultSet.getString(i++);
            String description = resultSet.getString(i++);
            String overviewSrc = resultSet.getString(i++);
	    /* Return the value object. */
            return new OverviewDTO(overviewId, name, description, overviewSrc);
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    public Collection<OverviewDTO> searchAll(Connection connection)
            throws InternalErrorException {
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;

        try {

	    /* Create "preparedStatement". */
            String queryString =
                    "SELECT overviewid, name, description, src FROM cds_overview";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Execute query. */
            resultSet = preparedStatement.executeQuery();

            Collection<OverviewDTO> overviewDTOs = new ArrayList<OverviewDTO>();
            if (!resultSet.next()) {
                return overviewDTOs;
            }
            do {
		/* Get results. */
                int i = 1;
                String overviewId = resultSet.getString(i++);
                String name = resultSet.getString(i++);
                String description = resultSet.getString(i++);
                String overviewSrc = resultSet.getString(i++);
                OverviewDTO overviewDTO =
                        new OverviewDTO(overviewId, name, description,overviewSrc);
                overviewDTOs.add(overviewDTO);
            } while (resultSet.next());

	    /* Return value objects. */
            return overviewDTOs;
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    public void insert(Connection connection, OverviewDTO overviewDTO)
            throws InternalErrorException {
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;

        try {
	    /* Create "preparedStatement". */
            String queryString = "INSERT INTO cds_overview (overviewid, name, description, src) VALUES (?, ?, ?, ?)";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setString(i++, overviewDTO.getIdOverview());
            preparedStatement.setString(i++, overviewDTO.getName());
            preparedStatement.setString(i++, overviewDTO.getDescription());
            preparedStatement.setString(i++, overviewDTO.getOverviewSrc());
	    
	    /* Execute query. */
            int insertedRows = preparedStatement.executeUpdate();

            if (insertedRows == 0) {
                throw new SQLException("Can not add row to table 'cds_overview'");
            }

            if (insertedRows > 1) {
                throw new SQLException("Duplicate row in table 'cds_overview'");
            }
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    public void update(Connection connection, OverviewDTO overviewDTO)
            throws InternalErrorException, InstanceNotFoundException {
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;

        try {
	    /* Create "preparedStatement". */
            String queryString = "UPDATE cds_overview SET name = ?, description = ?, src = ? WHERE overviewid = ?";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setString(i++, overviewDTO.getName());
            preparedStatement.setString(i++, overviewDTO.getDescription());
            preparedStatement.setString(i++, overviewDTO.getOverviewSrc());
            preparedStatement.setString(i++, overviewDTO.getIdOverview());

	    /* Execute query. */
            int updatedRows = preparedStatement.executeUpdate();

            if (updatedRows == 0) {
                throw new InstanceNotFoundException(
                        overviewDTO.getIdOverview(), OverviewDTO.class.getName());
            }

            if (updatedRows > 1) {
                throw new SQLException("Duplicate row in table 'cds_overview'");
            }
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    public void remove(Connection connection, String overviewId)
            throws InternalErrorException, InstanceNotFoundException {
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;

        try {
	    /* Create "preparedStatement". */
            String queryString = "DELETE FROM cds_overview WHERE overviewid = ?";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setString(i++, overviewId);

	    /* Execute query. */
            int deletedRows = preparedStatement.executeUpdate();
            if (deletedRows == 0) {
                throw new InstanceNotFoundException(overviewId, OverviewDTO.class.getName());
            }
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 2.0/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  2.0 (the 'License'); you may not use this file except in compliance with
 *  the License. You may obtain a copy of the License at
 *  http://www.mozilla.org/MPL/
 *
 *  Software distributed under the License is distributed on an 'AS IS' basis,
 *  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 *  for the specific language governing rights and limitations under the
 *  License.
 *
 *
 *  The Initial Developers of the Original Code are Iago Corbal and Rong Chen.
 *  Portions created by the Initial Developer are Copyright (C) 2012-2013
 *  the Initial Developer. All Rights Reserved.
 *
 *  Contributor(s):
 *
 * Software distributed under the License is distributed on an 'AS IS' basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 *  ***** END LICENSE BLOCK *****
 */