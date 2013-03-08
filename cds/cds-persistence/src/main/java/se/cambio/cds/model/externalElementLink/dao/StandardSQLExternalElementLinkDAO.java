package se.cambio.cds.model.externalElementLink.dao;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;

import se.cambio.cds.model.externalElementLink.dto.ExternalElementLinkDTO;
import se.cambio.cds.model.util.sql.GeneralOperations;
import se.cambio.cds.util.exceptions.InstanceNotFoundException;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.exceptions.ModelException;

/**
 * @author iago.corbal
 *
 */
public class StandardSQLExternalElementLinkDAO implements SQLExternalElementLinkDAO {

	public Collection<ExternalElementLinkDTO> search(Connection connection, String idExternalElement, String idDataSource) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {

			/* Create "preparedStatement". */
			String queryString = "SELECT idElement FROM externalElementLink WHERE idExternalElement = ? AND idDataSource = ?";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setString(i++, idExternalElement);
			preparedStatement.setString(i++, idDataSource);

			/* Execute query. */
			resultSet = preparedStatement.executeQuery();

			Collection<ExternalElementLinkDTO> externalElementLinkDTOs = new ArrayList<ExternalElementLinkDTO>();
			if (!resultSet.next()) {
				return externalElementLinkDTOs;
			}
			do {
				/* Get results. */
				i = 1;
				Integer idElement = resultSet.getInt(i++);

				ExternalElementLinkDTO ExternalElementLinkDTO = 
					new ExternalElementLinkDTO(idExternalElement, idDataSource, idElement);

				externalElementLinkDTOs.add(ExternalElementLinkDTO);

			} while (resultSet.next());

			/* Return value objects. */
			return externalElementLinkDTOs;
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}

	public Collection<ExternalElementLinkDTO> searchByElementId(Connection connection, Integer idElement) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {

			/* Create "preparedStatement". */
			String queryString = 
				"SELECT idExternalElement, idDataSource FROM externalElementLink WHERE idElement = ?";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setInt(i++, idElement);
			
			/* Execute query. */
			resultSet = preparedStatement.executeQuery();

			Collection<ExternalElementLinkDTO> externalElementLinkDTOs = new ArrayList<ExternalElementLinkDTO>();
			if (!resultSet.next()) {
				return externalElementLinkDTOs;
			}
			do {
				/* Get results. */
				i = 1;
				String idExternalElement = resultSet.getString(i++);
				String idDataSource = resultSet.getString(i++);

				ExternalElementLinkDTO ExternalElementLinkDTO = 
					new ExternalElementLinkDTO(idExternalElement, idDataSource, idElement);
				externalElementLinkDTOs.add(ExternalElementLinkDTO);
			} while (resultSet.next());

			/* Return value objects. */
			return externalElementLinkDTOs;
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}

	public void add(Connection connection, ExternalElementLinkDTO externalElementLinkDTO) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {

			/* Create "preparedStatement". */
			String queryString = "INSERT INTO externalElementLink (idExternalElement, idDataSource, idElement) VALUES (?, ?, ?)";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setString(i++, externalElementLinkDTO.getIdExternalElement());
			preparedStatement.setString(i++, externalElementLinkDTO.getIdDataSource());
			preparedStatement.setInt(i++, externalElementLinkDTO.getIdElement());

			/* Execute query. */
			int insertedRows = preparedStatement.executeUpdate();

			if (insertedRows == 0) {
				throw new SQLException("Can not add row to table 'externalElementLink'");
			}

			if (insertedRows > 1) {
				throw new SQLException("Duplicate row in table 'externalElementLink'");
			}
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}

	public void remove(Connection connection, ExternalElementLinkDTO externalElementLinkDTO) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {
			/* Create "preparedStatement". */
			String queryString = "DELETE FROM externalElementLink WHERE idExternalElement = ? AND idDataSource = ? AND idElement = ?";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setString(i++, externalElementLinkDTO.getIdExternalElement());
			preparedStatement.setString(i++, externalElementLinkDTO.getIdDataSource());
			preparedStatement.setInt(i++, externalElementLinkDTO.getIdElement());

			/* Execute query. */
			int deletedRows = preparedStatement.executeUpdate();
			if (deletedRows == 0) {
				throw new InstanceNotFoundException(
						externalElementLinkDTO.getIdExternalElement()+"-"+externalElementLinkDTO.getIdDataSource()+"-"+externalElementLinkDTO.getIdElement(),
						ExternalElementLinkDTO.class.getName());
			}
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}
	
	public int removeByDataSourceId(Connection connection, String idDataSource) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {
			/* Create "preparedStatement". */
			String queryString = "DELETE FROM externalElementLink WHERE idDataSource = ?";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setString(i++, idDataSource);

			/* Execute query. */
			int deletedRows = preparedStatement.executeUpdate();
			return deletedRows;
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
 *  Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  1.1 (the 'License'); you may not use this file except in compliance with
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