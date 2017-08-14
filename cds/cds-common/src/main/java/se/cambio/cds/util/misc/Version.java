package se.cambio.cds.util.misc;

import lombok.extern.slf4j.Slf4j;
import se.cambio.openehr.util.exceptions.MissingConfigurationParameterException;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

@Slf4j
public class Version {

    private static String VERSION_PROPERTIES_FILE = "build.properties";

    private static Version instance = null;
    private Map<Object, Object> parameters;

    private Version() {
        try {
            Class<Version> version =
                    Version.class;
            ClassLoader classLoader =
                    version.getClassLoader();
            InputStream inputStream =
                    classLoader.getResourceAsStream(VERSION_PROPERTIES_FILE);
            Properties properties = new Properties();
            properties.load(inputStream);
            inputStream.close();
            parameters = new HashMap<>(properties);
        } catch (Exception ex) {
            log.error("Error loading version", ex);
        }
    }

    private static String getParameter(String name)
            throws MissingConfigurationParameterException {
        return (String) getDelegate().parameters.get(name);
    }

    public static String getVersion() {
        return "v" + getVersionNum();
    }

    public static String getVersionNum() {
        try {
            return getParameter("projectVersion");
        } catch (MissingConfigurationParameterException ex) {
            log.error("Error getting project version", ex);
            return "";
        }
    }

    public static String getBuildDate() {
        try {
            return getParameter("buildDate");
        } catch (MissingConfigurationParameterException ex) {
            log.error("Error getting project build date", ex);
            return "";
        }
    }

    public static String getBuildNum() {
        try {
            String buildNum = getParameter("buildNum");
            if (buildNum != null && !buildNum.equals("${buildNumber}")) {
                return "build " + buildNum;
            } else {
                return null;
            }
        } catch (MissingConfigurationParameterException ex) {
            log.error("Error getting project build number", ex);
            return "";
        }
    }

    public static Version getDelegate() {
        if (instance == null) {
            instance = new Version();
        }
        return instance;
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