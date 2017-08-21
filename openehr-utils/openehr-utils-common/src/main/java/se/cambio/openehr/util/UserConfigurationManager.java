package se.cambio.openehr.util;

import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.InvalidParameterException;
import java.util.*;

import static java.lang.String.format;

public class UserConfigurationManager {

    public static final String ARCHETYPES_FOLDER = "ArchetypesFolder";
    public static final String TEMPLATES_FOLDER = "TemplatesFolder";
    public static final String GUIDELINES_FOLDER = "GuidesFolder";
    public static final String TERMINOLOGIES_FOLDER = "TerminologiesFolder";
    public static final String DOCUMENTS_FOLDER = "DocumentsFolder";
    public static final String CURRENT_DATE_TIME = "CurrentDateTime";
    public static final String LANGUAGE = "Messages/Language";
    public static final String COUNTRY = "Messages/Country";
    public static final String ACTIVE_RULE_ENGINE = "cds-execution.engine.active";
    private File DEFAULT_REPO_FOLDER = new File(System.getProperty("user.home"), "clinical-models");

    private Map<String, CmFolder> cmFolderMap = new HashMap<>();
    private String activeRuleEngine;
    private String language;
    private String country;
    private Date currentDateTime;
    private Map<String, String> pathMap;

    private Logger logger = LoggerFactory.getLogger(UserConfigurationManager.class);

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

    public CmFolder getDocumentsFolder() {
        return getCmFolder(DOCUMENTS_FOLDER, "docs");
    }


    public String getLanguage() {
        return language;
    }

    public void setLanguage(String language) {
        this.language = language;
    }

    public String getCountryCode() {
        return country;
    }

    public void setCountry(String country) {
        this.country = country;
    }

    public String getActiveRuleEngine() {
        return activeRuleEngine;
    }

    public void setActiveRuleEngine(String ruleEngine) {
        this.activeRuleEngine = ruleEngine;
    }

    public Date getCurrentDateTime() {
        if (currentDateTime == null) {
            return Calendar.getInstance().getTime();
        } else {
            return currentDateTime;
        }
    }

    public void setCurrentDateTime(Date currentDateTime) {
        this.currentDateTime = currentDateTime;
    }

    public void setPathMap(Map<String, String> pathMap) {
        this.pathMap = pathMap;
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
            String path = pathMap.get(parameterName);
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
        File configFile = new File(System.getProperty("user.home"), ".gdleditor/UserConfig.properties");
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
        } catch (Exception ex) {
            logger.error("Error saving", ex);
            throw new IllegalArgumentException(format("Error saving configuration file %s : %s", configFile.getAbsolutePath(), ex.getMessage()));
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