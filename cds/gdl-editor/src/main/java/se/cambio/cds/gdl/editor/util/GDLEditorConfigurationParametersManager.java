package se.cambio.cds.gdl.editor.util;

import java.io.InputStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;

import javax.naming.InitialContext;

import se.cambio.cds.util.exceptions.MissingConfigurationParameterException;

public final class GDLEditorConfigurationParametersManager {

    private static final String JNDI_PREFIX = "java:comp/env/";

    private static final String CONFIGURATION_FILE = "GDLEditor.properties";

    private static boolean usesJNDI;
    private static Map <Object,Object> parameters;

    static {
	/*         
	 * We use a synchronized map because it will be filled by using a 
	 * lazy strategy.
	 */             
	parameters = Collections.synchronizedMap(new HashMap<Object,Object>());
	try {
	    /* Read property file (if exists).*/    
	    Class<GDLEditorConfigurationParametersManager> configurationParametersManagerClass = 
		    GDLEditorConfigurationParametersManager.class;
	    ClassLoader classLoader =
		    configurationParametersManagerClass.getClassLoader();
	    InputStream inputStream =
		    classLoader.getResourceAsStream(CONFIGURATION_FILE);
	    Properties properties = new Properties();
	    properties.load(inputStream);
	    inputStream.close();

	    /* We have been able to read the file. */
	    usesJNDI = false;
	    parameters.putAll(properties);

	    System.out.println("*** Using '" + CONFIGURATION_FILE +
		    "' file for configuration ***");
	} catch (Exception e) {
	    /* We have not been able to read the file. */
	    usesJNDI = true;
	}
    }

    private GDLEditorConfigurationParametersManager() {}

    public static String getParameter(String name) 
	    throws MissingConfigurationParameterException {

	String value = (String) parameters.get(name);

	if (value == null) {
	    //System.out.println("Missing "+name);
	    if (usesJNDI) {
		try {
		    InitialContext initialContext = new InitialContext();
		    value = (String) initialContext.lookup(
			    JNDI_PREFIX + name);
		    parameters.put(name, value);
		} catch (Exception e) {
		    throw new MissingConfigurationParameterException(name);
		}
	    } else {
		throw new MissingConfigurationParameterException(name);

	    }
	}

	return value;

    }
    
    public static void loadParameters(Hashtable<Object,Object> usrConfig){
	parameters.putAll(usrConfig);
    }

    public static Object getObjectParameter(String name) 
	    throws MissingConfigurationParameterException {

	Object value = (Object) parameters.get(name);

	if (value == null) {
	    if (usesJNDI) {
		try {
		    InitialContext initialContext = new InitialContext();
		    value = (Object) initialContext.lookup(JNDI_PREFIX + name);
		    parameters.put(name, value);
		} catch (Exception e) {
		    throw new MissingConfigurationParameterException(name);
		}
	    } else {
		throw new MissingConfigurationParameterException(name);

	    }
	}
	return value;
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