package se.cambio.openehr.util;

import org.apache.log4j.Logger;
import se.cambio.openehr.util.exceptions.MissingConfigurationParameterException;
import se.cambio.openehr.util.misc.OpenEHRConfigurationParametersManager;

import javax.swing.*;
import java.io.*;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

public class UserConfigurationManager {

    public static String ARCHETYPES_FOLDER_KW = "ArchetypesFolder";
    public static String TEMPLATES_FOLDER_KW = "TemplatesFolder";
    public static String GUIDES_FOLDER_KW = "GuidesFolder";
    public static String TERMINOLOGIES_FOLDER_KW = "TerminologiesFolder";
    public static String ONTOLOGIES_FOLDER_KW = "OntologiesFolder";
    public static String DOCUMENTS_FOLDER_KW = "DocumentsFolder";
    public static String OVERVIEWS_FOLDER_KW = "OverviewsFolder";
    public static String CURRENT_DATE_TIME_KW = "CurrentDateTime";
    public static final String LANGUAGE = "Messages/Language";
    public static final String COUNTRY = "Messages/Country";

    public static final String DEFAULT_LANGUAGE = "en";
    public static final String DEFAULT_COUNTRY = "EN";

    private static final String CONFIGURATION_FOLDER = "conf";
    private static final String USER_CONFIGURATION_FOLDER = ".gdleditor";
    private static final String CONFIGURATION_FILE = "UserConfig.properties";
    private static Map <Object,Object> parameters;
    private static File _configFile = null;

    private static Map<String, String> _defaultValues = new HashMap<String, String>();
    static{
        _defaultValues.put(ARCHETYPES_FOLDER_KW, "Archetypes");
        _defaultValues.put(TEMPLATES_FOLDER_KW, "Templates");
        _defaultValues.put(GUIDES_FOLDER_KW, "Guidelines");
        _defaultValues.put(TERMINOLOGIES_FOLDER_KW, "Terminologies");
        _defaultValues.put(ONTOLOGIES_FOLDER_KW, "Ontologies");
        _defaultValues.put(OVERVIEWS_FOLDER_KW, "Overviews");
        _defaultValues.put(DOCUMENTS_FOLDER_KW, "docs");
        _defaultValues.put(CURRENT_DATE_TIME_KW, null);
        _defaultValues.put(LANGUAGE, DEFAULT_LANGUAGE);
        _defaultValues.put(COUNTRY, DEFAULT_COUNTRY);

	/*         
	 * We use a synchronized map because it will be filled by using a 
	 * lazy strategy.
	 */
        parameters = Collections.synchronizedMap(new HashMap<Object,Object>());
        try {
            InputStream is = null;
            _configFile = getConfigFile();
            if (_configFile==null){
                //Read from jar
                is = UserConfigurationManager.class.getClassLoader().getResourceAsStream(CONFIGURATION_FILE);
                if (is==null){
                    //User user configuration directory
                    String path = System.getProperty("user.home")+File.separator+USER_CONFIGURATION_FOLDER+File.separator+CONFIGURATION_FILE;
                    _configFile = new File(path);
                    is = new FileInputStream(_configFile);
                    Logger.getLogger(OpenEHRConfigurationParametersManager.class).info("*** Using user home folder for '"+CONFIGURATION_FILE+"'");
                }else{
                    Logger.getLogger(OpenEHRConfigurationParametersManager.class).info("*** Using resource for '"+CONFIGURATION_FILE+"'");
                }
            }else{
                is = new FileInputStream(_configFile);
                Logger.getLogger(OpenEHRConfigurationParametersManager.class).info("*** Using '"+CONFIGURATION_FOLDER+"' folder for '"+CONFIGURATION_FILE+"'");
            }
            if (is!=null){
                //We use an extended properties to be able to load paths defined with backslash (\)
                PropertiesEx properties = new PropertiesEx();
                properties.load(is);
                is.close();
                parameters.putAll(properties);
            }

        } catch (Exception e) {
            ExceptionHandler.handle(e);
        }
    }

    private static File getConfigFile(){
        try{
            File jarFile = new File(UserConfigurationManager.class.getProtectionDomain().getCodeSource().getLocation().getPath());
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
            //Loggr.getLogger(UserConfigurationManager.class).warn("CONF Folder not found "+t.getMessage());
        }
        try{
            //Current folder
            File file = new File(CONFIGURATION_FOLDER+File.separator+CONFIGURATION_FILE);
            if (file.exists()){
                return file;
            }

        }catch(Throwable t2){
            //Problem finding config folder
            //Logger.getLogger(UserConfigurationManager.class).warn("CONF Folder not found "+t.getMessage());
        }
        return null;
    }

    private UserConfigurationManager() {}

    public static String getParameter(String name)
            throws MissingConfigurationParameterException {
        String value = (String) parameters.get(name);
        if (value == null) {
            throw new MissingConfigurationParameterException(name);
        }
        return value;

    }

    public static void loadParameters(Hashtable<Object,Object> usrConfig){
        parameters.putAll(usrConfig);
    }

    public static Object getObjectParameter(String name)
            throws MissingConfigurationParameterException {
        Object value = (Object) parameters.get(name);
        if (value == null) {
            throw new MissingConfigurationParameterException(name);
        }
        return value;
    }

    public static void setParameter(String name, String value) {
        parameters.put(name, value);
    }

    public static String getParameterWithDefault(String keyword){
        String value = null;
        try {
            value = getParameter(keyword);
        } catch (MissingConfigurationParameterException e) {
        }
        if (value==null){
            return _defaultValues.get(keyword);
        }else{
            return value;
        }
    }

    public static Date getCustomDate(){
        try{
            String value = getParameterWithDefault(CURRENT_DATE_TIME_KW);
            if (value!=null){
                SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
                return dateFormat.parse(value);
            }
        }catch(ParseException e){}
        return null;
    }

    public static File getArchetypeFolder(){
        String folderStr = getParameterWithDefault(ARCHETYPES_FOLDER_KW);
        return new File(folderStr);
    }

    public static File getTemplateFolder(){
        String folderStr = getParameterWithDefault(TEMPLATES_FOLDER_KW);
        return new File(folderStr);
    }

    public static File getGuidesFolder(){
        String folderStr = getParameterWithDefault(GUIDES_FOLDER_KW);
        return new File(folderStr);
    }

    public static File getTerminologiesFolder(){
        String folderStr = getParameterWithDefault(TERMINOLOGIES_FOLDER_KW);
        return new File(folderStr);
    }

    public static File getOntologiesFolder(){
        String folderStr = getParameterWithDefault(ONTOLOGIES_FOLDER_KW);
        return new File(folderStr);
    }

    public static File getDocumentsFolder(){
        String folderStr = getParameterWithDefault(DOCUMENTS_FOLDER_KW);
        return new File(folderStr);
    }

    public static File getOverviewsFolder(){
        String folderStr = getParameterWithDefault(OVERVIEWS_FOLDER_KW);
        return new File(folderStr);
    }

    public static String getLanguage(){
        return getParameterWithDefault(LANGUAGE);
    }

    public static String getCountryCode(){
        return getParameterWithDefault(COUNTRY);
    }

    public static void setParameterWithDefault(String keyword, String value){
        setParameter(keyword, value);
    }

    public static boolean saveConfig(){
        try{
	    /*
	    Class<UserConfigurationManager> clazz = 
		    UserConfigurationManager.class;
	    ClassLoader classLoader =
		    clazz.getClassLoader();
	    String resourceFolder = classLoader.getResource(".").getPath();
	     */
            if (_configFile!=null){
                _configFile.createNewFile();
                OutputStream out = new FileOutputStream(_configFile);
                Properties properties = new Properties();
                properties.putAll(parameters);
                properties.store(out, "User Config");
            }
            return true;
        }catch(IOException e){
            ExceptionHandler.handle(e);
            JOptionPane.showMessageDialog(null, "Error saving config file", "Error", JOptionPane.ERROR_MESSAGE);
            return false;
        }
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