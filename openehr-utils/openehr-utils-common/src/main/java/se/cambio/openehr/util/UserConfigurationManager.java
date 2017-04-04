package se.cambio.openehr.util;

import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.env.Environment;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.InvalidParameterException;
import java.util.*;

import static java.lang.String.format;

@Configuration
@PropertySource(value = "file:conf/UserConfig.properties", ignoreResourceNotFound = true)
public class UserConfigurationManager {

    private static String ARCHETYPES_FOLDER = "ArchetypesFolder";
    private static String TEMPLATES_FOLDER = "TemplatesFolder";
    private static String GUIDELINES_FOLDER = "GuidesFolder";
    private static String TERMINOLOGIES_FOLDER = "TerminologiesFolder";
    private static String DOCUMENTS_FOLDER = "DocumentsFolder";
    private static String CURRENT_DATE_TIME = "CurrentDateTime";
    private static String LANGUAGE = "Messages/Language";
    private static String COUNTRY = "Messages/Country";
    private static String ACTIVE_RULE_ENGINE = "cds-execution.engine.active";
    private static List<String> SUPPORTED_RULE_ENGINES = Arrays.asList("rule-drools-engine", "rule-jgdl-engine");
    private static File DEFAULT_REPO_FOLDER = new File(System.getProperty("user.home"), "clinical-models");

    private Map<String, CmFolder> cmFolderMap = new HashMap<>();
    private String activeRuleEngine;
    private String language;
    private String country;
    private Date currentDateTime;
    private boolean currentDateTimeChecked = false;

    private Logger logger = LoggerFactory.getLogger(UserConfigurationManager.class);

    private static UserConfigurationManager instance;

    @Autowired
    private Environment environment;

    public UserConfigurationManager() {
    }

    public CmFolder getArchetypeFolder() {
        return getCmFolder(ARCHETYPES_FOLDER, "archetypes");
    }

    public CmFolder getTemplateFolder() {
        return getCmFolder(TEMPLATES_FOLDER, "templates");
    }

    public CmFolder getGuidesFolder() {
        return getCmFolder(GUIDELINES_FOLDER, "guidelines");
    }

    public CmFolder getTerminologiesFolder() {
        return getCmFolder(TERMINOLOGIES_FOLDER, "terminologies");
    }

    public String getLanguage() {
        if (language == null) {
            language = environment.getProperty(LANGUAGE, "en");
        }
        return language;
    }

    public void setLanguage(String language) {
        this.language = language;
    }

    public String getCountryCode() {
        if (country == null) {
            country = environment.getProperty(COUNTRY, "EN");
        }
        return country;
    }

    public void setCountry(String country) {
        this.country = country;
    }

    public String getActiveRuleEngine() {
        if (activeRuleEngine == null) {
            activeRuleEngine = environment.getProperty(ACTIVE_RULE_ENGINE, "rule-drools-engine");
        }
        return activeRuleEngine;
    }

    public List<String> getSupportedRuleEngines() {
        return SUPPORTED_RULE_ENGINES;
    }

    public void setActiveRuleEngine(String ruleEngine) {
        this.activeRuleEngine = ruleEngine;
    }

    public File getDocumentsFolder() {
        return new File(environment.getProperty(DOCUMENTS_FOLDER, "docs"));
    }

    public Date getCurrentDateTime() {
        if (!currentDateTimeChecked) {
            currentDateTimeChecked = true;
            String currentDateStr = environment.getProperty(CURRENT_DATE_TIME, "");
            if (currentDateStr.isEmpty()) {
                return Calendar.getInstance().getTime();
            } else {
                currentDateTime = new DateTime(currentDateStr).toDate();
            }
        }
        return currentDateTime;
    }

    public void setCurrentDateTime(Date currentDateTime) {
        this.currentDateTime = currentDateTime;
    }

    public boolean hasCustomCurrentDateTime() {
        getCurrentDateTime();
        return currentDateTime != null;
    }

    public void setArchetypesFolderPath(String archetypesFolderPath) {
        setCmFolder(ARCHETYPES_FOLDER, archetypesFolderPath);
    }

    public void setTemplatesFolderPath(String archetypesFolderPath) {
        setCmFolder(TEMPLATES_FOLDER, archetypesFolderPath);
    }

    public void setTerminologiesFolderPath(String archetypesFolderPath) {
        setCmFolder(TERMINOLOGIES_FOLDER, archetypesFolderPath);
    }

    public void setGuidelinesFolderPath(String archetypesFolderPath) {
        setCmFolder(GUIDELINES_FOLDER, archetypesFolderPath);
    }

    private void setCmFolder(String parameter, String path) {
        CmFolder cmFolder = getCmFolder(parameter, "");
        File folder = new File(path);
        folder.mkdirs();
        if (!folder.exists() || !folder.isDirectory()) {
            InvalidParameterException invalidParameterException = new InvalidParameterException(format("Invalid path '%s' given. Trying to set parameter %s", path, parameter));
            logger.error("Error setting cm folder", invalidParameterException);
            throw invalidParameterException;
        }
        cmFolder.setFolder(folder);
    }

    private CmFolder getCmFolder(String parameterName, String defaultValue) {
        CmFolder cmFolder = cmFolderMap.get(parameterName);
        if (cmFolder == null) {
            File folder;
            String path = environment.getProperty(parameterName);
            if (path != null) {
                File pathFile = new File(path);
                if (pathFile.isDirectory()) {
                    folder = pathFile;
                } else {
                    pathFile.mkdirs();
                    folder = pathFile;
                }
            } else {
                folder = new File(DEFAULT_REPO_FOLDER, defaultValue);
                folder.mkdirs();
            }
            cmFolder = new CmFolder(folder);
            cmFolderMap.put(parameterName, cmFolder);
        }
        return cmFolder;
    }

    public void saveConfig() {
        File configFile = new File("conf/UserConfig.properties");
        configFile.getParentFile().mkdirs();
        try {
            configFile.createNewFile();
            try (OutputStream out = new FileOutputStream(configFile)) {
                Properties properties = new Properties();
                properties.put(ARCHETYPES_FOLDER, getArchetypeFolder().getFolder().getAbsolutePath().replace("\\", "/"));
                properties.put(TEMPLATES_FOLDER, getTemplateFolder().getFolder().getAbsolutePath().replace("\\", "/"));
                properties.put(GUIDELINES_FOLDER, getGuidesFolder().getFolder().getAbsolutePath().replace("\\", "/"));
                properties.put(TERMINOLOGIES_FOLDER, getTerminologiesFolder().getFolder().getAbsolutePath().replace("\\", "/"));
                properties.put(LANGUAGE, getLanguage());
                properties.put(COUNTRY, getCountryCode());
                if (hasCustomCurrentDateTime()) {
                    properties.put(CURRENT_DATE_TIME, new DateTime(getCurrentDateTime()).toString());
                }
                properties.put(ACTIVE_RULE_ENGINE, getActiveRuleEngine());
                properties.store(out, "User Config");
            }
        } catch (Exception e) {
            logger.error("Error saving", e);
            throw new IllegalArgumentException(format("Error saving configuration file %s : %s", configFile.getAbsolutePath(), e.getMessage()));
        }
    }

    public static UserConfigurationManager instance() {
        if (instance == null) {
            instance = BeanProvider.getBean(UserConfigurationManager.class);
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