package se.cambio.cds.model.rule.dao;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;

import se.cambio.cds.model.rule.dto.RuleDTO;
import se.cambio.cds.model.util.sql.GeneralOperations;
import se.cambio.cds.util.exceptions.InstanceNotFoundException;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.exceptions.ModelException;

/**
 * @author iago.corbal
 *
 */
public class StandardSQLRuleDAO implements SQLRuleDAO {

    public RuleDTO search(Connection connection, String idRule) 
	    throws InternalErrorException, ModelException {
	PreparedStatement preparedStatement = null;
	ResultSet resultSet = null;

	try {

	    /* Create "preparedStatement". */
	    String queryString = "SELECT idGuide FROM rule WHERE idRule = ?";
	    preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
	    int i = 1;
	    preparedStatement.setString(i++, idRule);

	    /* Execute query. */
	    resultSet = preparedStatement.executeQuery();

	    if (!resultSet.next()) {
		throw new InstanceNotFoundException(idRule, RuleDTO.class.getName());
	    }

	    /* Get results. */
	    i = 1;
	    Integer idGuide = resultSet.getInt(i++);

	    /* Return the value object. */
	    return new RuleDTO(idRule, idGuide);
	} catch (SQLException e) {
	    throw new InternalErrorException(e);
	} finally {
	    GeneralOperations.closeResultSet(resultSet);
	    GeneralOperations.closeStatement(preparedStatement);
	}
    }

    public Collection<String> searchAllIds(Connection connection) 
	    throws InternalErrorException, ModelException {
	PreparedStatement preparedStatement = null;
	ResultSet resultSet = null;

	try {

	    /* Create "preparedStatement". */
	    String queryString = 
		    "SELECT idRule FROM rule";
	    preparedStatement = connection.prepareStatement(queryString);

	    /* Execute query. */
	    resultSet = preparedStatement.executeQuery();

	    Collection<String> ruleIds = new ArrayList<String>();
	    if (!resultSet.next()) {
		return ruleIds;
	    }
	    do {
		/* Get results. */
		int i = 1;
		String idRule = resultSet.getString(i++);
		
		ruleIds.add(idRule);
	    } while (resultSet.next());

	    /* Return value objects. */
	    return ruleIds;
	} catch (SQLException e) {
	    throw new InternalErrorException(e);
	} finally {
	    GeneralOperations.closeResultSet(resultSet);
	    GeneralOperations.closeStatement(preparedStatement);
	}
    }

    public void add(Connection connection, RuleDTO ruleDTO) 
	    throws InternalErrorException, ModelException {
	PreparedStatement preparedStatement = null;
	ResultSet resultSet = null;

	try {

	    /* Create "preparedStatement". */
	    String queryString = "INSERT INTO rule (idRule, idGuide) VALUES (?, ?)";
	    preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
	    int i = 1;
	    preparedStatement.setString(i++, ruleDTO.getIdRule());
	    preparedStatement.setInt(i++, ruleDTO.getIdGuide());

	    /* Execute query. */
	    int insertedRows = preparedStatement.executeUpdate();

	    if (insertedRows == 0) {
		throw new SQLException("Can not add row to table 'rule'");
	    }

	    if (insertedRows > 1) {
		throw new SQLException("Duplicate row in table 'rule'");
	    }
	} catch (SQLException e) {
	    throw new InternalErrorException(e);
	} finally {
	    GeneralOperations.closeResultSet(resultSet);
	    GeneralOperations.closeStatement(preparedStatement);
	}
    }

    public void remove(Connection connection, String idRule) 
    throws InternalErrorException, ModelException {
	PreparedStatement preparedStatement = null;
	ResultSet resultSet = null;

	try {
	    /* Create "preparedStatement". */
	    String queryString = "DELETE FROM rule WHERE idRule = ?";
	    preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
	    int i = 1;
	    preparedStatement.setString(i++, idRule);

	    /* Execute query. */
	    int deletedRows = preparedStatement.executeUpdate();
	    if (deletedRows == 0) {
		throw new InstanceNotFoundException(idRule,
			RuleDTO.class.getName());
	    }
	} catch (SQLException e) {
	    throw new InternalErrorException(e);
	} finally {
	    GeneralOperations.closeResultSet(resultSet);
	    GeneralOperations.closeStatement(preparedStatement);
	}
    }
    
    public void removeByGuideId(Connection connection, Integer idGuide) 
    throws InternalErrorException, ModelException {
	PreparedStatement preparedStatement = null;
	ResultSet resultSet = null;

	try {
	    /* Create "preparedStatement". */
	    String queryString = "DELETE FROM rule WHERE idGuide = ?";
	    preparedStatement = connection.prepareStatement(queryString);

	    /* Fill "preparedStatement". */
	    int i = 1;
	    preparedStatement.setInt(i++, idGuide);

	    /* Execute query. */
	    preparedStatement.executeUpdate();

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