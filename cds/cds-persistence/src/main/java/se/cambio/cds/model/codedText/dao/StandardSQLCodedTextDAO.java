package se.cambio.cds.model.codedText.dao;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;

import se.cambio.cds.model.codedText.dto.CodedTextDTO;
import se.cambio.cds.model.util.conversors.DBConversion;
import se.cambio.cds.model.util.sql.GeneralOperations;
import se.cambio.cds.util.exceptions.InstanceNotFoundException;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.exceptions.ModelException;

/**
 * @author iago.corbal
 *
 */
public class StandardSQLCodedTextDAO implements SQLCodedTextDAO {

	public Collection<CodedTextDTO> searchByElementIds(Connection connection, Collection<Integer> idElements) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {
			
			/* Create "preparedStatement". */
			String queryString = "SELECT idElement, text, description, index, archTerm FROM codedText WHERE idElement IN "+DBConversion.toSQLArrayString(idElements);
			preparedStatement = connection.prepareStatement(queryString);

			/* Execute query. */
			resultSet = preparedStatement.executeQuery();

			Collection<CodedTextDTO> codedTextDTOs = new ArrayList<CodedTextDTO>();
			if (!resultSet.next()) {
				return codedTextDTOs;
			}
			do {
				/* Get results. */
				int i = 1;
				Integer idElement = resultSet.getInt(i++);
				String text = resultSet.getString(i++);
				String description = resultSet.getString(i++);
				String index = resultSet.getString(i++);
				String archTerm = resultSet.getString(i++);

				CodedTextDTO CodedTextDTO = 
					new CodedTextDTO(idElement, text, description, index, archTerm);

				codedTextDTOs.add(CodedTextDTO);

			} while (resultSet.next());

			/* Return value objects. */
			return codedTextDTOs;
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}

	public void add(Connection connection, CodedTextDTO codedTextDTO) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {

			/* Create "preparedStatement". */
			String queryString = "INSERT INTO codedText (idElement, text, description, index, archTerm) VALUES (?, ?, ?, ?, ?)";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setInt(i++, codedTextDTO.getIdElement());
			preparedStatement.setString(i++, codedTextDTO.getText());
			preparedStatement.setString(i++, codedTextDTO.getDescription());
			preparedStatement.setString(i++, codedTextDTO.getIndex());
			preparedStatement.setString(i++, codedTextDTO.getArchTerm());

			/* Execute query. */
			int insertedRows = preparedStatement.executeUpdate();

			if (insertedRows == 0) {
				throw new SQLException("Can not add row to table 'codedText'");
			}

			if (insertedRows > 1) {
				throw new SQLException("Duplicate row in table 'codedText'");
			}
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}

	public void removeByElementId(Connection connection, Integer idElement) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {
			/* Create "preparedStatement". */
			String queryString = "DELETE FROM codedText WHERE idElement = ?";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setInt(i++, idElement);

			/* Execute query. */
			int deletedRows = preparedStatement.executeUpdate();
			if (deletedRows == 0) {
				throw new InstanceNotFoundException(idElement,
						CodedTextDTO.class.getName());
			}
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}
	
	public void remove(Connection connection, Integer idElement, String index) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {
			/* Create "preparedStatement". */
			String queryString = "DELETE FROM codedText WHERE idElement = ? AND index = ?";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setInt(i++, idElement);
			preparedStatement.setString(i++, index);

			/* Execute query. */
			int deletedRows = preparedStatement.executeUpdate();
			if (deletedRows == 0) {
				throw new InstanceNotFoundException(idElement+"-"+index,
						CodedTextDTO.class.getName());
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