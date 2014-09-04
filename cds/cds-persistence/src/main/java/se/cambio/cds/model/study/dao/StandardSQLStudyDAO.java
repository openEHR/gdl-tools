package se.cambio.cds.model.study.dao;

import se.cambio.cds.model.study.dto.StudyDTO;
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
public class StandardSQLStudyDAO implements SQLStudyDAO {
    @Override
    public StudyDTO searchByStudyId(Connection connection, String studyId)
            throws InternalErrorException, InstanceNotFoundException {
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;
        try {
	        /* Create "preparedStatement". */
            String queryString = "SELECT studySrc, lastUpdate FROM cds_study WHERE studyId = ?";
            preparedStatement = connection.prepareStatement(queryString);
	        /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setString(i++, studyId);
	        /* Execute query. */
            resultSet = preparedStatement.executeQuery();
            if (!resultSet.next()) {
                throw new InstanceNotFoundException(studyId, StudyDTO.class.getName());
            }
	        /* Get results. */
            i = 1;
            String studySrc = resultSet.getString(i++);
            Date lastUpdate = DBConversion.toDate(resultSet.getTimestamp(i++));
	        /* Return the value object. */
            return new StudyDTO(studyId, studySrc, lastUpdate);
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    @Override
    public Collection<StudyDTO> searchAll(Connection connection)
            throws InternalErrorException {
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;
        try {
	        /* Create "preparedStatement". */
            String queryString =
                    "SELECT studyid, studySrc, lastUpdate FROM cds_study";
            preparedStatement = connection.prepareStatement(queryString);
	        /* Execute query. */
            resultSet = preparedStatement.executeQuery();
            Collection<StudyDTO> studyDTOs = new ArrayList<StudyDTO>();
            if (!resultSet.next()) {
                return studyDTOs;
            }
            do {
		    /* Get results. */
                int i = 1;
                String studyId = resultSet.getString(i++);
                String studySrc = resultSet.getString(i++);
                Date lastUpdate = DBConversion.toDate(resultSet.getTimestamp(i++));
                StudyDTO studyDTO =
                        new StudyDTO(studyId, studySrc, lastUpdate);
                studyDTOs.add(studyDTO);
            } while (resultSet.next());
	        /* Return value objects. */
            return studyDTOs;
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    @Override
    public void upsert(Connection connection, StudyDTO studyDTO)
            throws InternalErrorException {
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;
        try {
	        /* Create "preparedStatement". */
            String queryString = "UPDATE cds_study SET studySrc = ?, lastUpdate = ? WHERE studyid = ?";
            preparedStatement = connection.prepareStatement(queryString);
	        /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setString(i++, studyDTO.getStudySrc());
            preparedStatement.setTimestamp(i++, DBConversion.toTimestamp(studyDTO.getLastUpdate()));
            preparedStatement.setString(i++, studyDTO.getStudyId());
	        /* Execute query. */
            int updatedRows = preparedStatement.executeUpdate();
            if (updatedRows > 1) {
                throw new SQLException("Duplicate row in table 'cds_study'");
            }
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeResultSet(resultSet);
            GeneralOperations.closeStatement(preparedStatement);
        }
    }

    @Override
    public void remove(Connection connection, String studyId)
            throws InternalErrorException, InstanceNotFoundException {
        PreparedStatement preparedStatement = null;
        ResultSet resultSet = null;
        try {
	        /* Create "preparedStatement". */
            String queryString = "DELETE FROM cds_study WHERE studyid = ?";
            preparedStatement = connection.prepareStatement(queryString);
	        /* Fill "preparedStatement". */
            int i = 1;
            preparedStatement.setString(i++, studyId);
	        /* Execute query. */
            int deletedRows = preparedStatement.executeUpdate();
            if (deletedRows == 0) {
                throw new InstanceNotFoundException(studyId, StudyDTO.class.getName());
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
            String queryString = "SELECT lastUpdate FROM cds_study ORDER BY lastUpdate DESC LIMIT 1";
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