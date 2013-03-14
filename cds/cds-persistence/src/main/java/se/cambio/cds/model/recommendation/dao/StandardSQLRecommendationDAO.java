package se.cambio.cds.model.recommendation.dao;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;

import se.cambio.cds.model.recommendation.dto.RecommendationDTO;
import se.cambio.cds.model.util.conversors.DBConversion;
import se.cambio.cds.model.util.sql.GeneralOperations;
import se.cambio.cds.util.exceptions.InstanceNotFoundException;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.exceptions.ModelException;

/**
 * @author iago.corbal
 *
 */
public class StandardSQLRecommendationDAO implements SQLRecommendationDAO {

	public RecommendationDTO search(Connection connection, Long idRecommendation)
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {
			/* Create "preparedStatement". */
			String queryString = 
				"SELECT idRule, idPatient, discardRepeatsPeriod, idUserGroup, idUser, idDecisionDeliveryChannel, address, level," +
				" msg, fireDate, state, responseDate, response" +
				" FROM recommendationHistory WHERE idRecommendation = ?";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setLong(i++, idRecommendation);

			/* Execute query. */
			resultSet = preparedStatement.executeQuery();

			if (!resultSet.next()) {
				throw new InstanceNotFoundException(idRecommendation, RecommendationDTO.class.getName());
			}

			/* Get results. */
			i = 1;
			String idRule = resultSet.getString(i++);
			String idPatient = resultSet.getString(i++);
			Integer discardRepeatsPeriod = resultSet.getInt(i++);
			String idUserGroup = resultSet.getString(i++);
			String idUser = resultSet.getString(i++);
			String idChannel = resultSet.getString(i++);
			String address = resultSet.getString(i++);
			Short level = resultSet.getShort(i++);
			String msg = resultSet.getString(i++);
			Calendar fireDate = DBConversion.toCalendar(resultSet.getTimestamp(i++));
			Short state = resultSet.getShort(i++);
			Calendar responseDate = DBConversion.toCalendar(resultSet.getTimestamp(i++));
			String response = resultSet.getString(i++);

			/* Return the value object. */
			return new RecommendationDTO(
					idRecommendation, idRule, idPatient, discardRepeatsPeriod, idUserGroup,
					idUser, idChannel, address, level, msg, fireDate, state, responseDate, response);
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}

	public Collection<RecommendationDTO> searchUnresolved(Connection connection) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {
			/* Create "preparedStatement". */
			String queryString = 
				"SELECT idRule, idPatient, discardRepeatsPeriod, idUserGroup, idUser, idDecisionDeliveryChannel, address, level," +
				" msg, fireDate, state, responseDate, response" +
				" FROM recommendationHistory" +
				" WHERE state == 0 AND level > 0";
			preparedStatement = connection.prepareStatement(queryString);

			/* Execute query. */
			resultSet = preparedStatement.executeQuery();

			Collection<RecommendationDTO> RecommendationDTOs = new ArrayList<RecommendationDTO>();
			if (!resultSet.next()) {
				return RecommendationDTOs;
			}
			do {
				/* Get results. */
				int i = 1;
				Long idRecommendation = resultSet.getLong(i++);
				String idRule = resultSet.getString(i++);
				String idPatient = resultSet.getString(i++);
				Integer discardRepeatsPeriod = resultSet.getInt(i++);
				String idUserGroup = resultSet.getString(i++);
				String idUser = resultSet.getString(i++);
				String idChannel = resultSet.getString(i++);
				String address = resultSet.getString(i++);
				Short level = resultSet.getShort(i++);
				String msg = resultSet.getString(i++);
				Calendar fireDate = DBConversion.toCalendar(resultSet.getTimestamp(i++));
				Short state = resultSet.getShort(i++);
				Calendar responseDate = DBConversion.toCalendar(resultSet.getTimestamp(i++));
				String response = resultSet.getString(i++);

				/* Return the value object. */
				RecommendationDTO recommendationDTO = new RecommendationDTO(
						idRecommendation, idRule, idPatient, discardRepeatsPeriod, idUserGroup,
						idUser, idChannel, address, level, msg, fireDate, state, responseDate, response);

				RecommendationDTOs.add(recommendationDTO);

			} while (resultSet.next());

			/* Return value objects. */
			return RecommendationDTOs;
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}


	public RecommendationDTO add(Connection connection, RecommendationDTO recommendationDTO) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {

			/* Get primary key */
			preparedStatement = 
				connection.prepareStatement("SELECT numericValue FROM paramconfig WHERE alphamericValue='recommendation' FOR UPDATE");
			resultSet = preparedStatement.executeQuery();
			if (!resultSet.next()) {
				throw new SQLException(
				"Error generating key on 'recommendation' table.");
			}
			Long idRecommendation = resultSet.getLong(1);
			preparedStatement = connection.prepareStatement("UPDATE paramconfig SET numericValue = numericValue + 1 WHERE alphamericValue='recommendation'");
			preparedStatement.executeUpdate();
			recommendationDTO.setIdRecommendation(idRecommendation);

			/* Create "preparedStatement". */
			String queryString = 
				"INSERT INTO recommendationHistory" +
				" (idRecommendation, idRule, idPatient, discardRepeatsPeriod, idUserGroup, idUser, idDecisionDeliveryChannel, address, level," +
				" msg, fireDate, state, responseDate, response) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setLong(i++, recommendationDTO.getIdRecommendation());
			preparedStatement.setString(i++, recommendationDTO.getIdRule());
			preparedStatement.setString(i++, recommendationDTO.getIdPatient());
			preparedStatement.setInt(i++, recommendationDTO.getDiscardRepeatsPeriod());
			if (recommendationDTO.getIdUserGroup()==null) {
				preparedStatement.setNull(i++, Types.VARCHAR);
			}else{
				preparedStatement.setString(i++, recommendationDTO.getIdUserGroup());
			}
			if (recommendationDTO.getIdUser()==null) {
				preparedStatement.setNull(i++, Types.VARCHAR);
			}else{
				preparedStatement.setString(i++, recommendationDTO.getIdUser());
			}
			preparedStatement.setString(i++, recommendationDTO.getIdDecisionDeliveryChannel());
			if (recommendationDTO.getAddress()==null) {
				preparedStatement.setNull(i++, Types.VARCHAR);
			}else{
				preparedStatement.setString(i++, recommendationDTO.getAddress());
			}
			preparedStatement.setShort(i++, recommendationDTO.getLevel());
			preparedStatement.setString(i++, recommendationDTO.getMsg());
			preparedStatement.setTimestamp(i++, DBConversion.toTimestamp(recommendationDTO.getFireDate()));
			preparedStatement.setShort(i++, recommendationDTO.getState());
			if (recommendationDTO.getResponseDate()==null) {
				preparedStatement.setNull(i++, Types.TIMESTAMP);
			}else{
				preparedStatement.setTimestamp(i++, DBConversion.toTimestamp(recommendationDTO.getResponseDate()));
			}
			if (recommendationDTO.getResponse()==null) {
				preparedStatement.setNull(i++, Types.VARCHAR);
			}else{
				preparedStatement.setString(i++, recommendationDTO.getResponse());
			}
			/* Execute query. */
			int insertedRows = preparedStatement.executeUpdate();

			if (insertedRows == 0) {
				throw new SQLException("Can not add row to table 'recommendationHistory'");
			}

			if (insertedRows > 1) {
				throw new SQLException("Duplicate row in table 'recommendationHistory'");
			}
			return recommendationDTO;
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}

	public void update(Connection connection, RecommendationDTO recommendationDTO) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {

			/* Create "preparedStatement". */
			String queryString = 
				"UPDATE recommendationHistory" +
				" SET idRule = ?, idPatient = ?, discardRepeatsPeriod = ?, idUserGroup = ?, idUser = ?, idDecisionDeliveryChannel = ?, address = ?, level = ?," +
				" msg = ?, fireDate = ?, state = ?, responseDate = ?, response = ? WHERE idRecommendation = ?";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;

			preparedStatement.setString(i++, recommendationDTO.getIdRule());
			preparedStatement.setString(i++, recommendationDTO.getIdPatient());
			preparedStatement.setInt(i++, recommendationDTO.getDiscardRepeatsPeriod());
			if (recommendationDTO.getIdUserGroup()==null) {
				preparedStatement.setNull(i++, Types.VARCHAR);
			}else{
				preparedStatement.setString(i++, recommendationDTO.getIdUserGroup());
			}
			if (recommendationDTO.getIdUser()==null) {
				preparedStatement.setNull(i++, Types.VARCHAR);
			}else{
				preparedStatement.setString(i++, recommendationDTO.getIdUser());
			}
			preparedStatement.setString(i++, recommendationDTO.getIdDecisionDeliveryChannel());
			if (recommendationDTO.getAddress()==null) {
				preparedStatement.setNull(i++, Types.VARCHAR);
			}else{
				preparedStatement.setString(i++, recommendationDTO.getAddress());
			}
			preparedStatement.setShort(i++, recommendationDTO.getLevel());
			preparedStatement.setString(i++, recommendationDTO.getMsg());
			preparedStatement.setTimestamp(i++, DBConversion.toTimestamp(recommendationDTO.getFireDate()));
			preparedStatement.setShort(i++, recommendationDTO.getState());
			if (recommendationDTO.getResponseDate()==null) {
				preparedStatement.setNull(i++, Types.TIMESTAMP);
			}else{
				preparedStatement.setTimestamp(i++, DBConversion.toTimestamp(recommendationDTO.getResponseDate()));
			}
			if (recommendationDTO.getResponse()==null) {
				preparedStatement.setNull(i++, Types.VARCHAR);
			}else{
				preparedStatement.setString(i++, recommendationDTO.getResponse());
			}
			preparedStatement.setLong(i++, recommendationDTO.getIdRecommendation());

			/* Execute query. */
			int updatedRows = preparedStatement.executeUpdate();

			if (updatedRows == 0) {
				throw new InstanceNotFoundException(
						recommendationDTO.getIdRecommendation(), 
						RecommendationDTO.class.getName());
			}

			if (updatedRows > 1) {
				throw new SQLException("Duplicate row in table 'recommendationHistory'");
			}
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeResultSet(resultSet);
			GeneralOperations.closeStatement(preparedStatement);
		}
	}

	public void remove(Connection connection, Long idRecommendation) 
	throws InternalErrorException, ModelException {
		PreparedStatement preparedStatement = null;
		ResultSet resultSet = null;

		try {
			/* Create "preparedStatement". */
			String queryString = "DELETE FROM recommendationHistory WHERE idRecommendation = ?";
			preparedStatement = connection.prepareStatement(queryString);

			/* Fill "preparedStatement". */
			int i = 1;
			preparedStatement.setLong(i++, idRecommendation);

			/* Execute query. */
			int deletedRows = preparedStatement.executeUpdate();
			if (deletedRows == 0) {
				throw new InstanceNotFoundException(idRecommendation,
						RecommendationDTO.class.getName());
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