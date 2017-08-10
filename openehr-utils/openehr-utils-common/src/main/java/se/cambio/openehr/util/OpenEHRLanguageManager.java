package se.cambio.openehr.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.openehr.util.configuration.UserConfiguration;
import se.cambio.openehr.util.misc.UTF8Control;

import java.util.*;

import static java.lang.String.format;


public final class OpenEHRLanguageManager {


    private static OpenEHRLanguageManager instance;

    private Map<String, ResourceBundle> resourceMap = null;
    private static final String MESSAGES_BUNDLE = "se.cambio.openehr.view.messages.Messages";
    private Logger logger = LoggerFactory.getLogger(OpenEHRLanguageManager.class);
    private String language;
    private String country;

    private OpenEHRLanguageManager() {
        UserConfigurationManager userConfigurationManager = UserConfiguration.getInstanceUserConfigurationManager();
        language = userConfigurationManager.getLanguage();
        country = userConfigurationManager.getCountryCode();
        resourceMap = new HashMap<>();
    }

    private ResourceBundle getResourceBundle() {
        return getResourceBundle(language);
    }

    private ResourceBundle getResourceBundle(String language) {
        if (language == null) {
            return getResourceBundle();
        }
        ResourceBundle resourceBundle = resourceMap.get(language);
        if (resourceBundle == null) {
            try {
                resourceBundle = ResourceBundle.getBundle(MESSAGES_BUNDLE, new Locale(language, country), new UTF8Control());
            } catch (Exception ex) {
                if (!this.language.equals(language)) {
                    resourceBundle = getResourceBundle();
                }
                logger.error(format("Error parsing resource for language '%s'", language), ex);
            }
            resourceMap.put(language, resourceBundle);
        }
        return resourceBundle;
    }

    public static void refreshConfig() {
        instance = null;
        getDelegate();
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
            return getDelegate().getResourceBundle(language).getString(key);
        } catch (MissingResourceException ex) {
            LoggerFactory.getLogger(OpenEHRLanguageManager.class).error(format("Error getting translation for label '%s' with language '%s'", key, language), ex);
            return "ERROR: Text not Found!";
        }
    }

    public static String getMessageWithLanguage(String key, String data1, String language) {
        String str = getDelegate().getResourceBundle(language).getString(key);
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
        String str = getDelegate().getResourceBundle(language).getString(key);
        for (int mainIndex = 0; mainIndex < data.length && mainIndex < 10; mainIndex++) {
            int index = str.indexOf("$" + mainIndex);
            String s1 = str.substring(0, index);
            String s2 = str.substring(index + 2, str.length());
            str = s1 + data[mainIndex] + s2;
        }
        return str;
    }

    private static OpenEHRLanguageManager getDelegate() {
        if (instance == null) {
            instance = new OpenEHRLanguageManager();
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