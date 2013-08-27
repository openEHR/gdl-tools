package se.cambio.cds.gdl.editor.view.applicationobjects;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

public class Countries {
    
    private static Countries _instance;
    private Map<String, String> _countries;
    
    private Countries(){
        //
        // A collection to store our country object
        //
        
 
        //
        // Get ISO countries, create Country object and
        // store in the collection.
        //
	_countries = new HashMap<String, String>();
        String[] isoCountries = Locale.getISOCountries();
        for (String country : isoCountries) {
            Locale locale = new Locale("en", country);
            String code = locale.getCountry();
            String name = locale.getDisplayCountry();
 
            if (!"".equals(code) && !"".equals(name)) {
        	_countries.put(code, name);
            }
        }
    }
    
    public static String getCountryName(String code){
	return getDelegate()._countries.get(code);
    }
 
    public static Countries getDelegate(){
	if (_instance==null){
	    _instance = new Countries();
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