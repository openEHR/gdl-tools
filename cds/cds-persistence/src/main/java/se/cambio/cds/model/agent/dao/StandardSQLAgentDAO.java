package se.cambio.cds.model.agent.dao;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;

import se.cambio.cds.model.agent.dto.AgentDTO;
import se.cambio.cds.model.util.conversors.DBConversion;
import se.cambio.cds.model.util.sql.GeneralOperations;
import se.cambio.cds.util.exceptions.InstanceNotFoundException;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.exceptions.ModelException;

/**
 * @author iago.corbal
 *
 */
public class StandardSQLAgentDAO implements SQLAgentDAO {

	public AgentDTO search(Connection connection, Integer idAgent) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {

			/* Create "preparedStatement". */
			String queryString = "SELECT idDataSource, period, active FROM agent WHERE idAgent = ?";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setInt(i++, idAgent);

			/* Execute query. */
			resultSet = preparedStatement.executeQuery();

			if (!resultSet.next()) {
				throw new InstanceNotFoundException(idAgent, AgentDTO.class.getName());
			}

			/* Get results. */
			i = 1;
			String idDataSource = resultSet.getString(i++);
			Integer period = resultSet.getInt(i++);
			Boolean active = DBConversion.toBoolean(resultSet.getShort(i++));

			/* Return the value object. */
			return new AgentDTO(idAgent, idDataSource, period, active);
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}

	public Collection<AgentDTO> searchAll(Connection connection) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {

			/* Create "preparedStatement". */
			String queryString = 
				"SELECT idAgent, idDataSource, period, active FROM agent";
			preparedStatement = connection.prepareStatement(queryString);

			/* Execute query. */
			resultSet = preparedStatement.executeQuery();

			Collection<AgentDTO> agentDTOs = new ArrayList<AgentDTO>();
			if (!resultSet.next()) {
				return agentDTOs;
			}
			do {
				/* Get results. */
				int i = 1;
				Integer idAgent = resultSet.getInt(i++);
				String idDataSource = resultSet.getString(i++);
				Integer period = resultSet.getInt(i++);
				Boolean active = DBConversion.toBoolean(resultSet.getShort(i++));
				
				AgentDTO AgentDTO = 
					new AgentDTO(idAgent, idDataSource, period, active);

				agentDTOs.add(AgentDTO);

			} while (resultSet.next());

			/* Return value objects. */
			return agentDTOs;
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}

	public AgentDTO add(Connection connection, AgentDTO agentDTO) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {
			/* Get primary key */
			preparedStatement = 
				connection.prepareStatement("SELECT numericValue FROM paramconfig WHERE alphamericValue='agent' FOR UPDATE");
			resultSet = preparedStatement.executeQuery();
			if (!resultSet.next()) {
				throw new SQLException(
				"Error generating key on 'agent' table.");
			}
			Integer idAgent = resultSet.getInt(1);
			preparedStatement = connection.prepareStatement("UPDATE paramconfig SET numericValue = numericValue + 1 WHERE alphamericValue='agent'");
			preparedStatement.executeUpdate();
			agentDTO.setIdAgent(idAgent);

			
			/* Create "preparedStatement". */
			String queryString = "INSERT INTO agent (idAgent, idDataSource, period) VALUES (?, ?, ?)";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setInt(i++, agentDTO.getIdAgent());
			preparedStatement.setString(i++, agentDTO.getIdDataSource());
			preparedStatement.setInt(i++, agentDTO.getPeriod());
			preparedStatement.setShort(i++, DBConversion.toShort(agentDTO.getActive()));
			
			/* Execute query. */
			int insertedRows = preparedStatement.executeUpdate();

			if (insertedRows == 0) {
				throw new SQLException("Can not add row to table 'agent'");
			}

			if (insertedRows > 1) {
				throw new SQLException("Duplicate row in table 'agent'");
			}
			return agentDTO;
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}

	public void update(Connection connection, AgentDTO agentDTO) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {
			/* Create "preparedStatement". */
			String queryString = "UPDATE agent SET idDataSource = ?, period = ?, active = ? WHERE idAgent = ?";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setString(i++, agentDTO.getIdDataSource());
			preparedStatement.setInt(i++, agentDTO.getPeriod());
			preparedStatement.setShort(i++, DBConversion.toShort(agentDTO.getActive()));
			preparedStatement.setInt(i++, agentDTO.getIdAgent());
			
			/* Execute query. */
			int updatedRows = preparedStatement.executeUpdate();

			if (updatedRows == 0) {
				throw new InstanceNotFoundException(
						agentDTO.getIdAgent(), 
						AgentDTO.class.getName());
			}

			if (updatedRows > 1) {
				throw new SQLException("Duplicate row in table 'agent'");
			}
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}
	
	public void remove(Connection connection, Integer idAgent) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {
			/* Create "preparedStatement". */
			String queryString = "DELETE FROM agent WHERE idAgent = ?";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setInt(i++, idAgent);

			/* Execute query. */
			int deletedRows = preparedStatement.executeUpdate();
			if (deletedRows == 0) {
				throw new InstanceNotFoundException(idAgent,
						AgentDTO.class.getName());
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