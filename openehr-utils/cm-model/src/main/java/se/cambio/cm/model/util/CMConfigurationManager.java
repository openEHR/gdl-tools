package se.cambio.cm.model.util;

import se.cambio.openehr.util.PropertiesEx;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.MissingConfigurationParameterException;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class CMConfigurationManager {

    private static final String CONFIGURATION_FILE = "cm-config.properties";
    private static Map<Object, Object> parameters;

    static {
        try {
            parameters = Collections.synchronizedMap(new HashMap<>());
            InputStream is = CMConfigurationManager.class.getClassLoader().getResourceAsStream(CONFIGURATION_FILE);
            if (is != null) {
                PropertiesEx properties = new PropertiesEx();
                properties.load(is);
                is.close();
                parameters.putAll(properties);
            }
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    static List<CMType> getAdditionalCMElements() throws MissingConfigurationParameterException, InternalErrorException {
        List<CMType> cmTypes = new ArrayList<>();
        List<String> cmElementIds = getAdditionalCMElementsIds();
        for (String cmElementId : cmElementIds) {
            Class clazz = getAdditionalCMElementClass(cmElementId);
            List<String> extensions = getAdditionalCMElementExtensions(cmElementId);
            cmTypes.add(new CMType(cmElementId, clazz, extensions));
        }
        return cmTypes;
    }

    private static List<String> getAdditionalCMElementsIds() {
        String value = (String) parameters.get("AdditionalCMElements");
        List<String> cmElementIds = new ArrayList<String>();
        if (value != null) {
            String[] cmElementIdsAux = value.split(",");
            for (String cmElementId : cmElementIdsAux) {
                cmElementIds.add(cmElementId.trim());
            }
        }
        return cmElementIds;
    }

    private static Class getAdditionalCMElementClass(String cmElementId) throws MissingConfigurationParameterException, InternalErrorException {
        String parameterName = cmElementId + "/Class";
        String value = (String) parameters.get(parameterName);
        if (value == null) {
            throw new MissingConfigurationParameterException(parameterName);
        }
        try {
            return Class.forName(value);
        } catch (ClassNotFoundException ex) {
            throw new InternalErrorException(ex);
        }
    }

    private static List<String> getAdditionalCMElementExtensions(String cmElementId) {
        String value = (String) parameters.get(cmElementId + "/Extensions");
        List<String> extensions = new ArrayList<>();
        if (value != null) {
            String[] extensionsAux = value.split(",");
            for (String extension : extensionsAux) {
                extensions.add(extension.trim());
            }
        }
        return extensions;
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