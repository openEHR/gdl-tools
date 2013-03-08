package se.cambio.cds.model.dataSource.dao;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;

import se.cambio.cds.model.dataSource.dto.DataSourceDTO;
import se.cambio.cds.model.util.sql.GeneralOperations;
import se.cambio.cds.util.exceptions.InstanceNotFoundException;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.exceptions.ModelException;

/**
 * @author iago.corbal
 *
 */
public class StandardSQLDataSourceDAO implements SQLDataSourceDAO {

	public DataSourceDTO search(Connection connection, String idDataSource) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {
			/* Create "preparedStatement". */
			String queryString = "SELECT description FROM dataSource WHERE idDataSource = ?";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setString(i++, idDataSource);

			/* Execute query. */
			resultSet = preparedStatement.executeQuery();

			if (!resultSet.next()) {
				throw new InstanceNotFoundException(idDataSource, DataSourceDTO.class.getName());
			}

			/* Get results. */
			i = 1;
			String description = resultSet.getString(i++);

			/* Return the value object. */
			return new DataSourceDTO(idDataSource, description);
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}

	public Collection<DataSourceDTO> searchAll(Connection connection) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {

			/* Create "preparedStatement". */
			String queryString = 
				"SELECT idDataSource, description FROM dataSource";
			preparedStatement = connection.prepareStatement(queryString);

			/* Execute query. */
			resultSet = preparedStatement.executeQuery();

			Collection<DataSourceDTO> dataSourceDTOs = new ArrayList<DataSourceDTO>();
			if (!resultSet.next()) {
				return dataSourceDTOs;
			}
			do {
				/* Get results. */
				int i = 1;
				String idDataSource = resultSet.getString(i++);
				String description = resultSet.getString(i++);

				DataSourceDTO DataSourceDTO = 
					new DataSourceDTO(idDataSource, description);

				dataSourceDTOs.add(DataSourceDTO);

			} while (resultSet.next());

			/* Return value objects. */
			return dataSourceDTOs;
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}

	public void add(Connection connection, DataSourceDTO dataSourceDTO) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {

			/* Create "preparedStatement". */
			String queryString = "INSERT INTO dataSource (idDataSource, description) VALUES (?, ?)";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setString(i++, dataSourceDTO.getIdDataSource());
			preparedStatement.setString(i++, dataSourceDTO.getDescription());

			/* Execute query. */
			int insertedRows = preparedStatement.executeUpdate();

			if (insertedRows == 0) {
				throw new SQLException("Can not add row to table 'dataSource'");
			}

			if (insertedRows > 1) {
				throw new SQLException("Duplicate row in table 'dataSource'");
			}
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}

	public void remove(Connection connection, String idDataSource) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {
			/* Create "preparedStatement". */
			String queryString = "DELETE FROM dataSource WHERE idDataSource = ?";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setString(i++, idDataSource);

			/* Execute query. */
			int deletedRows = preparedStatement.executeUpdate();
			if (deletedRows == 0) {
				throw new InstanceNotFoundException(idDataSource,
						DataSourceDTO.class.getName());
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