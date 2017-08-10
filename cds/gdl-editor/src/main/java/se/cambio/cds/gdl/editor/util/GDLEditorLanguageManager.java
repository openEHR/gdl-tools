package se.cambio.cds.gdl.editor.util;

import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.configuration.UserConfiguration;
import se.cambio.openehr.util.misc.UTF8Control;

import java.util.Locale;
import java.util.ResourceBundle;


public final class GDLEditorLanguageManager {


    private static GDLEditorLanguageManager instance;

    private ResourceBundle resource = null;
    private static final String MESSAGES_BUNDLE = "se.cambio.cds.gdl.editor.view.messages.Messages";

    private GDLEditorLanguageManager() {
        UserConfigurationManager userConfigurationManager = UserConfiguration.getInstanceUserConfigurationManager();
        String language = userConfigurationManager.getLanguage();
        String country = userConfigurationManager.getCountryCode();
        resource = ResourceBundle.getBundle(MESSAGES_BUNDLE, new Locale(language, country), new UTF8Control());
    }

    public static String getMessage(String key) {
        return getDelegate().resource.getString(key);
    }

    public static String getMessage(String key, String data1) {
        String s = getDelegate().resource.getString(key);
        int i = s.indexOf("$0");
        if (i >= 0 && i < s.length()) {
            String s1 = s.substring(0, i);
            String s2 = s.substring(i + 2, s.length());
            return s1 + data1 + s2;
        } else return s;
    }

    public static String getMessage(String key, String[] data) {
        String s = getDelegate().resource.getString(key);
        for (int i = 0; i < data.length && i < 10; i++) {
            int index = s.indexOf("$" + i);
            String s1 = s.substring(0, index);
            String s2 = s.substring(index + 2, s.length());
            s = s1 + data[i] + s2;
        }
        return s;
    }

    private static GDLEditorLanguageManager getDelegate() {
        if (instance == null) {
            instance = new GDLEditorLanguageManager();
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