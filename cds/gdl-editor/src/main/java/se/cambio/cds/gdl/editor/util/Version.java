/*
 * Creado el 12-sep-2007
 *


 */
package se.cambio.cds.gdl.editor.util;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.MissingConfigurationParameterException;

/**
 * @author icorram
 *


 */
public class Version {

    private static String VERSION_PROPPERTIES_FILE = "Version.properties";

    private static Version _instance = null;
    private Map<Object, Object> parameters;

    private Version(){
	try {
	    Class<Version> version = 
		    Version.class;
	    ClassLoader classLoader =
		    version.getClassLoader();
	    InputStream inputStream =
		    classLoader.getResourceAsStream(VERSION_PROPPERTIES_FILE);
	    Properties properties = new Properties();
	    properties.load(inputStream);
	    inputStream.close();
	    parameters = new HashMap<Object,Object>(properties);

	} catch (Exception e) {
	    ExceptionHandler.handle(e);
	}
    }

    private static String getParameter(String name) 
	    throws MissingConfigurationParameterException {
	return (String) getDelegate().parameters.get(name);
    }

    public static String getVersion(){
	return "v"+getVersionNum();
    }

    public static String getVersionNum(){
	try {
	    return getParameter("projectVersion");
	} catch (MissingConfigurationParameterException e) {
	    ExceptionHandler.handle(e);
	    return "";
	}
    }

    public static String getBuildDate(){
	try {
	    return getParameter("buildDate");
	} catch (MissingConfigurationParameterException e) {
	    ExceptionHandler.handle(e);
	    return "";
	}
    }

    public static String getBuildNum(){
	try {
	    String buldNum = getParameter("buildNum");
	    if (buldNum!=null && !buldNum.equals("${buildNumber}")){
		return "build "+buldNum;
	    }else{
		return null;
	    }
	} catch (MissingConfigurationParameterException e) {
	    ExceptionHandler.handle(e);
	    return "";
	}
    }

    public static Version getDelegate(){
	if (_instance==null){
	    _instance = new Version();
	}
	return _instance;
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