package se.cambio.cds.ts;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.text.CodePhrase;

import se.cambio.cds.util.exceptions.InternalErrorException;

public class TerminologyServiceConfiguration {

    // private static String TERMINOLOGY_FOLDER = "Terminologies"; 

    public TerminologyServiceConfiguration(
	    //Set<String> supportedTerminologies,
	    //Map<String, String> terminologySources,
	    Map<String, String> terminologyURLs,
	    //Map<String, String> terminologyPluginSources,
	    Map<String, String> terminologyPluginSourcesClass) {
	super();
	//this.supportedTerminologies = supportedTerminologies;
	//this.terminologySources = terminologySources;
	this.terminologyURLs = terminologyURLs;
	//this.terminologyPluginSources = terminologyPluginSources;
	this.terminologyPluginSourcesClass = terminologyPluginSourcesClass;		
	this.supportedLanguages = new HashSet<CodePhrase>();

	// TODO hardcoded language supports
	this.supportedLanguages.add(new CodePhrase("ISO_639-1", "en"));
	this.supportedLanguages.add(new CodePhrase("ISO_639-1", "sv"));
	this.supportedLanguages.add(new CodePhrase("ISO_639-1", "es"));
    }

    public static TerminologyServiceConfiguration loadFromProperties(InputStream input) throws InternalErrorException {

	Properties config = new Properties();
	try{
	    if (input!=null){
		config.load(input);
	    }else{
		Logger.getLogger(TerminologyServiceConfiguration.class).warn("Terminology configuration not found");
	    }
	}catch(IOException e){
	    throw new InternalErrorException(e);
	}
	Set<String> keys = config.stringPropertyNames();
	Map<String, String> URLs = new HashMap<String, String>();
	Map<String, String> pluginClass = new HashMap<String, String>();

	for (String key : keys) {
	    int i = key.lastIndexOf("_");
	    String terminology = key.substring(0, i);
	    String value = config.getProperty(key);
	    /*if (key.endsWith("_src")) {
		terminologies.add(terminology);
		sources.put(terminology, value);
	    } else */
	    if (key.endsWith("_url")) {
		URLs.put(terminology, value);
		/*} else if (key.endsWith("_plugin.src.file")) {
		pluginSources.put(terminology, value);*/
	    } else if(key.endsWith("_plugin.src.class")) {
		pluginClass.put(terminology, value);
		log.debug("Class: " + value);
	    }			
	}
	return new TerminologyServiceConfiguration(URLs, pluginClass);
    }

    /*
    public boolean terminologySupported(String terminology) {
	return supportedTerminologies.contains(terminology);
    }*/

    public boolean languageSupported(CodePhrase language) {
	return supportedLanguages.contains(language);
    }

    /*
    public Set<String> listSupportedTerminologies() {
	return Collections.unmodifiableSet(supportedTerminologies);
    }

    public String terminologySource(String terminology) {
	return terminologySources.get(terminology);
    }*/

    public String terminologyURL(String terminology) {
	return terminologyURLs.get(terminology);
    }

    /*
    public Map<String, String> getTerminologyPluginSources() {
	return terminologyPluginSources;
    }*/

    public String getPluginSourceClass(String terminology) {
	return terminologyPluginSourcesClass.get(terminology);
    }

    //private Set<String> supportedTerminologies;
    private Set<CodePhrase> supportedLanguages;
    //private Map<String, String> terminologySources;
    //private Map<String, String> terminologyPluginSources;
    private Map<String, String> terminologyPluginSourcesClass;
    private Map<String, String> terminologyURLs;
    private static Logger log = Logger.getLogger(TerminologyServiceConfiguration.class);	
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