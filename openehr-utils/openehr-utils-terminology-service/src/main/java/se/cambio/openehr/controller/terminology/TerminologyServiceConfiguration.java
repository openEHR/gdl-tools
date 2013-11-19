package se.cambio.openehr.controller.terminology;

import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.text.CodePhrase;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.MissingConfigurationParameterException;
import se.cambio.openehr.util.misc.OpenEHRConfigurationParametersManager;

import javax.naming.InitialContext;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.*;

public class TerminologyServiceConfiguration {

    private static final String JNDI_PREFIX = "java:comp/env/";
    private static final String CONFIGURATION_FILE = "TerminologyServiceConfig.properties";
    private static final String CONFIGURATION_FOLDER = "conf";
    private static TerminologyServiceConfiguration _delegate = null;
    private Set<CodePhrase> supportedLanguages;
    private Map<String, String> terminologyPluginSourcesClass;
    private Map<String, String> terminologyURLs;
    private static Logger log = Logger.getLogger(TerminologyServiceConfiguration.class);
    private static boolean usesJNDI;
    private static Map <Object,Object> parameters;

    static {
	/*         
	 * We use a synchronized map because it will be filled by using a 
	 * lazy strategy.
	 */
        parameters = Collections.synchronizedMap(new HashMap<Object,Object>());
        try {
	    /* Read property file (if exists).*/
            Class<OpenEHRConfigurationParametersManager> configurationParametersManagerClass =
                    OpenEHRConfigurationParametersManager.class;
            ClassLoader classLoader =
                    configurationParametersManagerClass.getClassLoader();
            File configFile = getConfigFile();
            InputStream inputStream = null;
            if (configFile!=null){
                inputStream = new FileInputStream(configFile);
                Logger.getLogger(OpenEHRConfigurationParametersManager.class).info("*** Using '"+CONFIGURATION_FOLDER+"' folder for '"+CONFIGURATION_FILE+"'");
            }else{
                inputStream = classLoader.getResourceAsStream(CONFIGURATION_FILE);
                Logger.getLogger(OpenEHRConfigurationParametersManager.class).info("*** Using resource for '"+CONFIGURATION_FILE+"'");
            }
            Properties properties = new Properties();
            properties.load(inputStream);
            inputStream.close();

	    /* We have been able to read the file. */
            usesJNDI = false;
            parameters.putAll(properties);
        } catch (Exception e) {
	    /* We have not been able to read the file. */
            usesJNDI = true;
        }
    }

    private TerminologyServiceConfiguration() {
        super();
        // TODO hardcoded language supports
        this.supportedLanguages = new HashSet<CodePhrase>();
        this.supportedLanguages.add(new CodePhrase("ISO_639-1", "en"));
        this.supportedLanguages.add(new CodePhrase("ISO_639-1", "sv"));
        this.supportedLanguages.add(new CodePhrase("ISO_639-1", "es"));
        try {
            loadFromProperties();
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        } catch (MissingConfigurationParameterException e) {
            ExceptionHandler.handle(e);
        }
    }

    private static File getConfigFile(){
        try{
            File jarFile = new File(TerminologyServiceConfiguration.class.getProtectionDomain().getCodeSource().getLocation().getPath());
            //../conf
            for (File file:jarFile.getParentFile().getParentFile().listFiles()){
                if (file.isDirectory() && file.getName().equals(CONFIGURATION_FOLDER)){
                    for (File file2:file.listFiles()){
                        if (file2.getName().equals(CONFIGURATION_FILE)){
                            return file2;
                        }
                    }
                }
            }
        }catch(Throwable t){
            //Problem finding config folder
            //Logger.getLogger(TerminologyServiceConfiguration.class).warn("CONF Folder not found "+t.getMessage());
        }
        return null;
    }

    private void loadFromProperties() throws InternalErrorException, MissingConfigurationParameterException {
        Set<Object> keys = parameters.keySet();
        terminologyURLs = new HashMap<String, String>();
        terminologyPluginSourcesClass = new HashMap<String, String>();

        for (Object key : keys) {
            if (key instanceof String){
                String keyStr = (String)key;
                int i = keyStr.lastIndexOf("_");
                String terminology = keyStr.substring(0, i);
                String value = getParameter(keyStr);
		/*if (key.endsWith("_src")) {
		terminologies.add(terminology);
		sources.put(terminology, value);
	    } else */
                if (keyStr.endsWith("_url")) {
                    terminologyURLs.put(terminology, value);
		    /*} else if (key.endsWith("_plugin.src.file")) {
		pluginSources.put(terminology, value);*/
                } else if(keyStr.endsWith("_plugin.src.class")) {
                    terminologyPluginSourcesClass.put(terminology, value);
                }
            }
        }
    }

    public static String getParameter(String name)
            throws MissingConfigurationParameterException {

        String value = (String) parameters.get(name);

        if (value == null) {
            if (usesJNDI) {
                try {
                    InitialContext initialContext = new InitialContext();
                    value = (String) initialContext.lookup(
                            JNDI_PREFIX + name);
                    parameters.put(name, value);
                } catch (Exception e) {
                    throw new MissingConfigurationParameterException(name);
                }
            } else {
                throw new MissingConfigurationParameterException(name);
            }
        }
        return value;
    }     

    /*
    public boolean terminologySupported(String terminology) {
	return supportedTerminologies.contains(terminology);
    }*/

    public static boolean languageSupported(CodePhrase language) {
        return getDelegate().supportedLanguages.contains(language);
    }

    /*
    public Set<String> listSupportedTerminologies() {
	return Collections.unmodifiableSet(supportedTerminologies);
    }

    public String terminologySource(String terminology) {
	return terminologySources.get(terminology);
    }*/

    public static String terminologyURL(String terminology) {
        return getDelegate().terminologyURLs.get(terminology);
    }

    /*
    public Map<String, String> getTerminologyPluginSources() {
	return terminologyPluginSources;
    }*/

    public static String getPluginSourceClass(String terminology) {
        return getDelegate().terminologyPluginSourcesClass.get(terminology);
    }

    public static TerminologyServiceConfiguration getDelegate(){
        if(_delegate==null){
            _delegate = new TerminologyServiceConfiguration();
        }
        return _delegate;
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