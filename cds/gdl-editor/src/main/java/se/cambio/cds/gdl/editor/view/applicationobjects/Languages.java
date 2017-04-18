package se.cambio.cds.gdl.editor.view.applicationobjects;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

public class Languages {

    private static Languages _instance;
    private Map<String, String> _languages;
    private Set<String> _supportedLanguages;

    private Languages() {
        _supportedLanguages = new HashSet<>();
        _supportedLanguages.add("en_EN");
        _supportedLanguages.add("es_ES");
        _supportedLanguages.add("sv_SE");
        _supportedLanguages.add("el_GR");

        _languages = new HashMap<>();
        String[] isoLanguages = Locale.getISOLanguages();
        for (String language : isoLanguages) {
            Locale locale = new Locale(language, "EN");
            String code = locale.getLanguage();
            String name = locale.getDisplayLanguage();

            if (!"".equals(code) && !"".equals(name)) {
                _languages.put(code, name);
            }
        }
    }

    public static Set<String> getSupportedLanguages() {
        return getDelegate()._supportedLanguages;
    }

    public static String getLanguageName(String code) {
        return getDelegate()._languages.get(code);
    }

    private static Languages getDelegate() {
        if (_instance == null) {
            _instance = new Languages();
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