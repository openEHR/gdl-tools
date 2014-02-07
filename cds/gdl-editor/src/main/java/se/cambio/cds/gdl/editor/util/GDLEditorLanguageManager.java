package se.cambio.cds.gdl.editor.util;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.MissingConfigurationParameterException;
import se.cambio.openehr.util.misc.UTF8Control;



public final class GDLEditorLanguageManager {


    private static GDLEditorLanguageManager _instance;

    private ResourceBundle _resource = null;
    public static final String MESSAGES_BUNDLE = "Messages/ResourceBundle";
    public String _lng = null;

    private GDLEditorLanguageManager(){
	_lng = UserConfigurationManager.getLanguage();
	String country = UserConfigurationManager.getCountryCode();
	try {
	    String msgBundle = GDLEditorConfigurationParametersManager.getParameter(MESSAGES_BUNDLE);
	    _resource = ResourceBundle.getBundle(msgBundle,new Locale(_lng,country), new UTF8Control());
	} catch (MissingConfigurationParameterException e) {
	    ExceptionHandler.handle(e);
	}
    }

    public static void refreshConfig(){
	_instance = null;
	getDelegate();
    }

    public static String getMessage(String key) {
	try {
	    return getDelegate()._resource.getString(key);
	} catch (MissingResourceException e) {
	    ExceptionHandler.handle(e);
	    return "ERROR: Text not Found!";
	}
    }

    public static String getMessage(String key,String data1) {
	String s = getDelegate()._resource.getString(key);
	int i = s.indexOf("$0");
	if (i>=0&&i<s.length()){
	    String s1 = s.substring(0,i);
	    String s2 = s.substring(i+2,s.length());
	    return s1+data1+s2;
	}else return s;
    }

    public static String getMessage(String key,String [] data) {
	String s = getDelegate()._resource.getString(key);
	for (int i=0;i<data.length && i<10;i++){
	    int index = s.indexOf("$"+i);
	    String s1 = s.substring(0,index);
	    String s2 = s.substring(index+2,s.length());
	    s = s1+data[i]+s2;
	}
	return s;
    }

    private static GDLEditorLanguageManager getDelegate() {
	if (_instance==null){
	    _instance = new GDLEditorLanguageManager();
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