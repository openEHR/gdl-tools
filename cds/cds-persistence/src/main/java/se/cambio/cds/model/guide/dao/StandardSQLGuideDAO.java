package se.cambio.cds.model.guide.dao;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;

import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.model.util.sql.GeneralOperations;
import se.cambio.cds.util.exceptions.GuideNotFoundException;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.exceptions.ModelException;

/**
 * @author iago.corbal
 *
 */
public class StandardSQLGuideDAO implements SQLGuideDAO {

	public GuideDTO search(Connection connection, String idGuide) 
	throws InternalErrorException, GuideNotFoundException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {
			/* Create "preparedStatement". */
			String queryString = "SELECT name, description, guideSrc, compiledGuide FROM guide WHERE idGuide = ?";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setString(i++, idGuide);

			/* Execute query. */
			resultSet = preparedStatement.executeQuery();

			if (!resultSet.next()) {
				throw new GuideNotFoundException(idGuide);
			}

			/* Get results. */
			i = 1;
			String name = resultSet.getString(i++);
			String desc = resultSet.getString(i++);
			String guideSrc = resultSet.getString(i++);
			byte[] compiledGuide = resultSet.getBytes(i++);

			/* Return the value object. */
			return new GuideDTO(idGuide, name, desc, guideSrc, compiledGuide);
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}

	public Collection<GuideDTO> searchAll(Connection connection) 
	throws InternalErrorException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {

			/* Create "preparedStatement". */
			String queryString = 
				"SELECT idGuide, name, description, guideSrc, compiledGuide FROM guide";
			preparedStatement = connection.prepareStatement(queryString);

			/* Execute query. */
			resultSet = preparedStatement.executeQuery();

			Collection<GuideDTO> guideDTOs = new ArrayList<GuideDTO>();
			if (!resultSet.next()) {
				return guideDTOs;
			}
			do {
				/* Get results. */
				int i = 1;
				String name = resultSet.getString(i++);
				String desc = resultSet.getString(i++);
				String idGuide = resultSet.getString(i++);
				String guideSrc = resultSet.getString(i++);
				byte[] compiledGuide = resultSet.getBytes(i++);
				
				GuideDTO GuideDTO = 
					new GuideDTO(idGuide, name, desc, guideSrc, compiledGuide);

				guideDTOs.add(GuideDTO);

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

	public GuideDTO add(Connection connection, GuideDTO guideDTO) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {
			/* Create "preparedStatement". */
			String queryString = "INSERT INTO guide (idGuide, name, description, guideSrc, compiledGuide) VALUES (?, ?, ?)";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setString(i++, guideDTO.getIdGuide());
			preparedStatement.setString(i++, guideDTO.getName());
			preparedStatement.setString(i++, guideDTO.getDescription());
			preparedStatement.setString(i++, guideDTO.getGuideSrc());
			preparedStatement.setBytes(i++, guideDTO.getCompiledGuide());

			/* Execute query. */
			int insertedRows = preparedStatement.executeUpdate();

			if (insertedRows == 0) {
				throw new SQLException("Can not add row to table 'guide'");
			}

			if (insertedRows > 1) {
				throw new SQLException("Duplicate row in table 'guide'");
			}
			return guideDTO;
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}

	public void update(Connection connection, GuideDTO guideDTO) 
	throws InternalErrorException, GuideNotFoundException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {
			/* Create "preparedStatement". */
			String queryString = "UPDATE guide SET name = ?, description = ?, guideSrc = ?, compiledGuide = ? WHERE idGuide = ?";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setString(i++, guideDTO.getName());
			preparedStatement.setString(i++, guideDTO.getDescription());
			preparedStatement.setString(i++, guideDTO.getGuideSrc());
			preparedStatement.setBytes(i++, guideDTO.getCompiledGuide());
			preparedStatement.setString(i++, guideDTO.getIdGuide());
			
			/* Execute query. */
			int updatedRows = preparedStatement.executeUpdate();

			if (updatedRows == 0) {
				throw new GuideNotFoundException(
						guideDTO.getIdGuide());
			}

			if (updatedRows > 1) {
				throw new SQLException("Duplicate row in table 'guide'");
			}
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}
	
	public void remove(Connection connection, String idGuide) 
	throws InternalErrorException, GuideNotFoundException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {
			/* Create "preparedStatement". */
			String queryString = "DELETE FROM guide WHERE idGuide = ?";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setString(i++, idGuide);

			/* Execute query. */
			int deletedRows = preparedStatement.executeUpdate();
			if (deletedRows == 0) {
				throw new GuideNotFoundException(idGuide);
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