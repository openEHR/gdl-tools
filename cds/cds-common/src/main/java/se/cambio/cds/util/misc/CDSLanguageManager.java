package se.cambio.cds.util.misc;

import lombok.extern.slf4j.Slf4j;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.configuration.UserConfiguration;
import se.cambio.openehr.util.misc.UTF8Control;

import java.util.*;

import static java.lang.String.format;

@Slf4j
public final class CDSLanguageManager {


    private static CDSLanguageManager instance;

    private Map<String, ResourceBundle> resourceMap = null;
    private static final String MESSAGES_BUNDLE = "se.cambio.cds.view.messages.Messages";
    private static String language;
    private static String country;

    private CDSLanguageManager() {
        UserConfigurationManager userConfigurationManager = UserConfiguration.getInstanceUserConfigurationManager();
        language = userConfigurationManager.getLanguage();
        country = userConfigurationManager.getCountryCode();
        resourceMap = new HashMap<>();
    }

    private static ResourceBundle getResourceBundle() {
        return getResourceBundle(language);
    }

    private static ResourceBundle getResourceBundle(String language) {
        ResourceBundle resourceBundle = getDelegate().resourceMap.get(language);
        if (resourceBundle == null) {
            try {
                resourceBundle = ResourceBundle.getBundle(MESSAGES_BUNDLE, new Locale(language, country), new UTF8Control());
            } catch (Exception ex) {
                if (!CDSLanguageManager.language.equals(language)) {
                    resourceBundle = getResourceBundle();
                }
                log.error("Error getting resource bundle for language: " + language, ex);
            }
            if (resourceBundle != null) {
                getDelegate().resourceMap.put(language, resourceBundle);
            } else {
                throw new RuntimeException(format("Cannot find resource bundle for language: %s", language));
            }
        }
        return resourceBundle;
    }

    public static String getMessage(String key) {
        return getMessageWithLanguage(key, getDelegate().getLanguage());
    }

    public static String getMessage(String key, String data1) {
        return getMessageWithLanguage(key, data1, getDelegate().getLanguage());
    }

    public static String getMessage(String key, String[] data) {
        return getMessageWithLanguage(key, data, getDelegate().getLanguage());
    }

    public static String getMessageWithLanguage(String key, String language) {
        try {
            return getResourceBundle(language).getString(key);
        } catch (MissingResourceException ex) {
            log.error(format("Error getting resource bundle for language '%s' with key '%s'", language, key));
            return "ERROR: Text not Found!";
        }
    }

    public static String getMessageWithLanguage(String key, String data1, String language) {
        String str = getResourceBundle(language).getString(key);
        int index = str.indexOf("$0");
        if (index >= 0 && index < str.length()) {
            String s1 = str.substring(0, index);
            String s2 = str.substring(index + 2, str.length());
            return s1 + data1 + s2;
        } else {
            return str;
        }
    }

    public static String getMessageWithLanguage(String key, String[] data, String language) {
        String str = getResourceBundle(language).getString(key);
        for (int index2 = 0; index2 < data.length && index2 < 10; index2++) {
            int index = str.indexOf("$" + index2);
            String s1 = str.substring(0, index);
            String s2 = str.substring(index + 2, str.length());
            str = s1 + data[index2] + s2;
        }
        return str;
    }

    private static CDSLanguageManager getDelegate() {
        if (instance == null) {
            instance = new CDSLanguageManager();
        }
        return instance;
    }

    public String getLanguage() {
        return language;
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