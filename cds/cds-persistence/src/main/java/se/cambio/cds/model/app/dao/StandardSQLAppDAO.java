package se.cambio.cds.model.app.dao;

import se.cambio.cds.model.app.dto.CDSAppDTO;
import se.cambio.openehr.model.util.conversors.DBConversion;
import se.cambio.openehr.model.util.sql.GeneralOperations;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

/**
 * @author iago.corbal
 *
 */
public class StandardSQLAppDAO implements SQLAppDAO {

    public CDSAppDTO searchByCDSAppId(Connection connection, String cdsAppId)
            throws InternalErrorException, InstanceNotFoundException {
        
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;

        try {
	    /* Create "preparedStatement". */
            String queryString = "SELECT appSrc, lastUpdate FROM cds_app WHERE cdsAppId = ?";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setString(i++, cdsAppId);

	    /* Execute query. */
            resultSet = preparedStatement.executeQuery();

            if (!resultSet.next()) {
                throw new InstanceNotFoundException(CDSAppDTO.class.getName(), cdsAppId);
            }

	    /* Get results. */
            i = 1;
            String appSrc = resultSet.getString(i++);
            Date lastUpdate = DBConversion.toDate(resultSet.getTimestamp(i++));
	    /* Return the value object. */
            return new CDSAppDTO(cdsAppId, appSrc, lastUpdate);
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    public Collection<CDSAppDTO> searchAll(Connection connection)
            throws InternalErrorException {
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;
        try {
	    /* Create "preparedStatement". */
            String queryString =
                    "SELECT cdsAppId, appSrc, lastUpdate FROM cds_app";
            preparedStatement = connection.prepareStatement(queryString);
	    /* Execute query. */
            resultSet = preparedStatement.executeQuery();
            Collection<CDSAppDTO> guideDTOs = new ArrayList<CDSAppDTO>();
            if (!resultSet.next()) {
                return guideDTOs;
            }
            do {
		/* Get results. */
                int i = 1;
                String cdsAppId = resultSet.getString(i++);
                String appSrc = resultSet.getString(i++);
                Date lastUpdate = DBConversion.toDate(resultSet.getTimestamp(i++));
                CDSAppDTO guideDTO =
                        new CDSAppDTO(cdsAppId, appSrc, lastUpdate);
                guideDTOs.add(guideDTO);
            } while (resultSet.next());

	    /* Return value objects. */
            return guideDTOs;
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }


    public CDSAppDTO insert(Connection connection, CDSAppDTO cdsAppDTO)
            throws InternalErrorException {
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;

        try {
	    /* Create "preparedStatement". */
            String queryString = "INSERT INTO cds_app (cdsAppId, appSrc, lastUpdate) VALUES (?, ?, ?)";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setString(i++, cdsAppDTO.getCdaAppId());
            preparedStatement.setString(i++, cdsAppDTO.getAppSrc());
            preparedStatement.setTimestamp(i++, DBConversion.toTimestamp(cdsAppDTO.getLastUpdate()));
	    /* Execute query. */
            int insertedRows = preparedStatement.executeUpdate();

            if (insertedRows == 0) {
                throw new SQLException("Can not add row to table 'cds_app'");
            }

            if (insertedRows > 1) {
                throw new SQLException("Duplicate row in table 'cds_app'");
            }
            return cdsAppDTO;
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    public void update(Connection connection, CDSAppDTO cdsAppDTO)
            throws InternalErrorException, InstanceNotFoundException {
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;

        try {
	    /* Create "preparedStatement". */
            String queryString = "UPDATE cds_app SET appSrc = ?, lastUpdate = ? WHERE cdsAppId = ?";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setString(i++, cdsAppDTO.getAppSrc());
            preparedStatement.setTimestamp(i++, DBConversion.toTimestamp(cdsAppDTO.getLastUpdate()));
            preparedStatement.setString(i++, cdsAppDTO.getCdaAppId());

	    /* Execute query. */
            int updatedRows = preparedStatement.executeUpdate();

            if (updatedRows == 0) {
                throw new InstanceNotFoundException(CDSAppDTO.class.getName(), cdsAppDTO.getCdaAppId());
            }

            if (updatedRows > 1) {
                throw new SQLException("Duplicate row in table 'cds_app'");
            }
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    public void remove(Connection connection, String cdsAppId)
            throws InternalErrorException, InstanceNotFoundException {
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;

        try {
	    /* Create "preparedStatement". */
            String queryString = "DELETE FROM cds_app WHERE cdsAppId = ?";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setString(i++, cdsAppId);

	    /* Execute query. */
            int deletedRows = preparedStatement.executeUpdate();
            if (deletedRows == 0) {
                throw new InstanceNotFoundException(CDSAppDTO.class.getName(), cdsAppId);
            }
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    @Override
    public Date getLastUpdateDate(Connection connection) throws InternalErrorException {
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;

        try {
	    /* Create "preparedStatement". */
            String queryString = "SELECT lastUpdate FROM cds_app ORDER BY lastUpdate DESC LIMIT 1";
            preparedStatement = connection.prepareStatement(queryString);

	    /* Execute query. */
            resultSet = preparedStatement.executeQuery();
            if (!resultSet.next()) {
                return null;
            }else {
                return DBConversion.toDate(resultSet.getTimestamp(1));
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