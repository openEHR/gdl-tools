package se.cambio.openehr.model.util.sql;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import se.cambio.openehr.util.exceptions.InternalErrorException;

/**
 * @author iago.corbal
 * 
 * Utility class with basic operations to close connections, results and SQL statements.
 * 
 */
public final class GeneralOperations {

    private GeneralOperations() {}
    
    /**
     * It closes a <code>ResultSet</code> if not <code>null</code>.
     */
    public static void closeResultSet(ResultSet resultSet) 
        throws InternalErrorException {
    	try {
    		if (resultSet != null) {        
    			resultSet.close();
    		}
    	} catch (SQLException e) {
    		throw new InternalErrorException(e);
    	}
    }

    /**
     * It closes a <code>Statement</code> if not <code>null</code>.
     */
    public static void closeStatement(Statement statement) 
    throws InternalErrorException {
    	try {
    		if (statement != null) {        
    			statement.close(); 
    		}
    	} catch (SQLException e) {
    		throw new InternalErrorException(e);
    	}        
    }
    
    /**
     * It closes a <code>Connection</code> if not <code>null</code>.
     */
    public static void closeConnection(Connection connection)
        throws InternalErrorException {
    	try {
    		if (connection != null && !connection.isClosed()) {          
    			connection.close();
    		}
    	} catch (SQLException e) {
    		throw new InternalErrorException(e);
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