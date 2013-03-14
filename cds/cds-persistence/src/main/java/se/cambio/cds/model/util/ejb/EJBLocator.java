package se.cambio.cds.model.util.ejb;

import java.io.InputStream;
import java.util.Properties;

import javax.naming.InitialContext;
import javax.naming.NamingException;

import org.omg.CORBA.ORB;

import se.cambio.cds.util.exceptions.InternalErrorException;

public final class EJBLocator {

    private final static String SERVER_CONFIGURATION_FILE = "EJBHomeLocatorJNDIInitialContext.properties";
    //private final static String PRODUCTION_ENVIROMENT = "production.server.url";
    //private final static String TEST_ENVIROMENT = "test.server.url";
    //private final static String JAVA_NAMING_URL = "java.naming.provider.url";

    private static Properties initialContextProperties;
    private static boolean usesConfigurationFile;

    static {

	/* 
	 * Check if a file for configuring JNDI InitialContext can be loaded 
	 * from the classpath. This situation is common when using
	 * EJBHomeLocator from the client tier to get a reference to a 
	 * EJBHome object residing in a separate container.
	 *
	 * If the file can not be loaded, JNDI InitialContext can be created
	 * with the no-args constructor. This situation is common when using
	 * EJBLocator inside the same container containing the EJBHome
	 * object.
	 */
	try {

	    Class<EJBLocator> EJBLocatorClass = EJBLocator.class;
	    ClassLoader classLoader = EJBLocatorClass.getClassLoader();
	    InputStream inputStream = classLoader.getResourceAsStream(
		    SERVER_CONFIGURATION_FILE);

	    initialContextProperties = new Properties();
	    initialContextProperties.load(inputStream);
	    inputStream.close();
	    usesConfigurationFile = true;

	    //Definimos los archivos de propiedades de la seguridad JAAS
	    //TODO Esto probablemente no funcione en otros SO
	    /*
	     * Activar para seguridad JAAS (junto con el AuthUsuarioPerfilFacadeDelegate)
	     */
	    /*
            String sasPath = classLoader.getResource(CONFIGURATION_SAS_CLIENT).toExternalForm();
            sasPath = sasPath.replace("%20", " ");
            String wsJaasPath = classLoader.getResource(CONFIGURATION_WSJAAS_CLIENT).toExternalForm();
            wsJaasPath = wsJaasPath.replace("%20", " ");

            System.setProperty(
            		"com.ibm.CORBA.ConfigURL",sasPath);
            System.setProperty(
            		"java.security.auth.login.config",wsJaasPath);
	     */
	    //System.setProperty("org.omg.CORBA.ORBInitialHost", initialContextProperties.getProperty("org.omg.CORBA.ORBInitialHost"));
	    //System.setProperty("org.omg.CORBA.ORBInitialPort", initialContextProperties.getProperty("org.omg.CORBA.ORBInitialPort"));
	    System.setProperty(
		    "com.ibm.CORBA.CommTrace","false");
	    System.setProperty(
		    "com.ibm.CORBA.Debug","false");
	} catch (Exception e) {
	    usesConfigurationFile = false;
	}
    }

    private EJBLocator() {}

    public final static Object getEJBObject(String jndiName) 
	    throws NamingException, InternalErrorException {

	InitialContext initialContext = getInitialContext();
	Object ejbObject = initialContext.lookup(jndiName);


	return ejbObject;
    }

    public final static void initOrbSecurity(){
	// initialize the ORB object
	Properties props = new Properties();
	ORB orb = ORB.init((String[]) null, props);

	// this is a dummy call to server to establish security realm for JAAS.
	// it should be done before the JAAS login
	if (usesConfigurationFile){
	    orb.string_to_object(initialContextProperties.getProperty("java.naming.provider.url"));
	}
    }

    private static InitialContext getInitialContext() throws NamingException {

	if (usesConfigurationFile) {
	    return new InitialContext(initialContextProperties);
	} else {
	    return new InitialContext();
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