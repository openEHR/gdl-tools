package se.cambio.openehr.model.util.sql;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.naming.InitialContext;
import javax.sql.DataSource;

import org.h2.jdbcx.JdbcDataSource;

import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.MissingConfigurationParameterException;
import se.cambio.openehr.util.misc.OpenEHRConfigurationParametersManager;

/**
 * Caches references to <code>DataSources</code>. It allows to register
 * explicitally a <code>DataSource</code> under a name, and get a reference to
 * it by its name (using JNDI, if not already registered or cached).
 */
public class DataSourceLocator {

    /**
     * The recommended JNDI context name for all <code>DataSources</code>:
     * <code>java:comp/env/jdbc/</code>
     */
    public static final String JNDI_PREFIX = "java:comp/env/";
    public static final String STANDALONE_DS = "standalone_ds";
    public static final String STANDALONE_DS_URL = "StandaloneDS/URL";

    private static Map<String, DataSource> dataSources = Collections.synchronizedMap(new HashMap<String, DataSource>());

    private DataSourceLocator() {}

    /**
     * Allows to register explicitally a <code>DataSource</code> with a
     * given name. This method should only be used when 
     * <code>DataSources</code> are not accessible through JNDI (for example 
     * when using an application server providing only servlets and JSP).
     */
    public static void addDataSource(String name, DataSource dataSource) {
	dataSources.put(name, dataSource);    
    }

    /**
     * Gets a reference to the <code>DataSource</code> with the given 
     * <code>name</code>. If the reference has been explicitally registered 
     * (with <code>addDataSource(String, DataSource)</code>) or is cached, it 
     * is returned immediatelly. Otherwise, JNDI is used to get a reference to
     * it (caching it for the next time) using the JNDI name:
     * <code>JNDI_PREFIX +  name</code>. This allows an easy transition from
     * an application server not providing <code>DataSources</code> registered
     * under JNDI to one providing them.
     */

    public static DataSource getDataSource(String name) 
	    throws InternalErrorException{
	DataSource dataSource = (DataSource) dataSources.get(name);
	if (dataSource == null) {
	    if (isStandalone()){
		dataSource = createStandaloneDataSource(name);
	    }else{
		dataSource = createICDataSource(name);
	    }
	    dataSources.put(name, dataSource);
	}
	return dataSource;
    }


    public static DataSource createICDataSource(String name) 
	    throws InternalErrorException{
	DataSource dataSource = null;
	try {
	    InitialContext initialContext = new InitialContext();
	    dataSource = (DataSource) initialContext.lookup(name);
	    dataSources.put(name, dataSource);
	} catch (Exception e) {
	    throw new InternalErrorException(e);
	}
	return dataSource;
    }

    public static DataSource createStandaloneDataSource(String name) throws InternalErrorException{
	DataSource dataSource = dataSources.get(STANDALONE_DS);
	if (dataSource==null){
	    JdbcDataSource dataSourceAux = new JdbcDataSource();
	    String standAloneURL;
	    try {
		standAloneURL = OpenEHRConfigurationParametersManager.getParameter(STANDALONE_DS_URL);
	    } catch (MissingConfigurationParameterException e) {
		throw new InternalErrorException(e);
	    }
	    dataSourceAux.setURL(standAloneURL);
	    dataSourceAux.setUser("sa");
	    dataSource = dataSourceAux;
	    dataSources.put(STANDALONE_DS, dataSource);
	}
	return dataSource;
    }

    public static boolean isStandalone(){
	try {
	    OpenEHRConfigurationParametersManager.getParameter(STANDALONE_DS_URL);
	} catch (MissingConfigurationParameterException e) {
	    return false;
	}
	return true;
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