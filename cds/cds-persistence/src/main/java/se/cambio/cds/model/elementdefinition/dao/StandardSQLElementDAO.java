package se.cambio.cds.model.elementdefinition.dao;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;

import se.cambio.cds.model.elementdefinition.dto.ElementDTO;
import se.cambio.cds.model.util.sql.GeneralOperations;
import se.cambio.cds.util.exceptions.InstanceNotFoundException;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.exceptions.ModelException;

/**
 * @author iago.corbal
 *
 */
public class StandardSQLElementDAO implements SQLElementDAO {

	public ElementDTO search(Connection connection, Integer idElement) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {

			/* Create "preparedStatement". */
			String queryString = "SELECT description, dataType, idParentElement, archetype, path FROM element WHERE idElement = ?";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setInt(i++, idElement);

			/* Execute query. */
			resultSet = preparedStatement.executeQuery();

			if (!resultSet.next()) {
				throw new InstanceNotFoundException(idElement, ElementDTO.class.getName());
			}

			/* Get results. */
			i = 1;
			String description = resultSet.getString(i++);
			String dataType = resultSet.getString(i++);
			Integer idParentElement = resultSet.getInt(i++);
			String archetype = resultSet.getString(i++);
			String path = resultSet.getString(i++);
			
			/* Return the value object. */
			return new ElementDTO(idElement, description, dataType, idParentElement, archetype, path);
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}

	public Collection<ElementDTO> searchAll(Connection connection) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {

			/* Create "preparedStatement". */
			String queryString = 
				"SELECT idElement, description, dataType, idParentElement, archetype, path FROM element";
			preparedStatement = connection.prepareStatement(queryString);

			/* Execute query. */
			resultSet = preparedStatement.executeQuery();

			Collection<ElementDTO> elementDTOs = new ArrayList<ElementDTO>();
			if (!resultSet.next()) {
				return elementDTOs;
			}
			do {
				/* Get results. */
				int i = 1;
				Integer idElement = resultSet.getInt(i++);
				String description = resultSet.getString(i++);
				String dataType = resultSet.getString(i++);
				Integer idParentElement = resultSet.getInt(i++);
				String archetype = resultSet.getString(i++);
				String path = resultSet.getString(i++);

				ElementDTO ElementDTO = 
					new ElementDTO(idElement, description, dataType, idParentElement, archetype, path);
				elementDTOs.add(ElementDTO);
			} while (resultSet.next());

			/* Return value objects. */
			return elementDTOs;
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}

	public ElementDTO add(Connection connection, ElementDTO elementDTO) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {
			/* Get primary key */
			preparedStatement = 
				connection.prepareStatement("SELECT numericValue FROM paramconfig WHERE alphamericValue='element' FOR UPDATE");
			resultSet = preparedStatement.executeQuery();
			if (!resultSet.next()) {
				throw new SQLException(
				"Error generating key on 'element' table.");
			}
			Integer idElement = resultSet.getInt(1);
			preparedStatement = connection.prepareStatement("UPDATE paramconfig SET numericValue = numericValue + 1 WHERE alphamericValue='element'");
			preparedStatement.executeUpdate();
			elementDTO.setIdElement(idElement);
			
			/* Create "preparedStatement". */
			String queryString = "INSERT INTO element (idElement, description, dataType, idParentElement, archetype, path) VALUES (?, ?, ?, ?, ?, ?)";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setInt(i++, elementDTO.getIdElement());
			preparedStatement.setString(i++, elementDTO.getDescription());
			preparedStatement.setString(i++, elementDTO.getDataType());
			preparedStatement.setInt(i++, elementDTO.getIdParentElement());
			preparedStatement.setString(i++, elementDTO.getArchetype());
			preparedStatement.setString(i++, elementDTO.getPath());
			
			/* Execute query. */
			int insertedRows = preparedStatement.executeUpdate();

			if (insertedRows == 0) {
				throw new SQLException("Can not add row to table 'element'");
			}

			if (insertedRows > 1) {
				throw new SQLException("Duplicate row in table 'element'");
			}
			return elementDTO;
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}

	public void remove(Connection connection, Integer idElement) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {
			/* Create "preparedStatement". */
			String queryString = "DELETE FROM element WHERE idElement = ?";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setInt(i++, idElement);

			/* Execute query. */
			int deletedRows = preparedStatement.executeUpdate();
			if (deletedRows == 0) {
				throw new InstanceNotFoundException(idElement,
						ElementDTO.class.getName());
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