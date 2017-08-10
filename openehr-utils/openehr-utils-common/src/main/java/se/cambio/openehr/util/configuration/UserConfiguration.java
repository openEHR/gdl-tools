package se.cambio.openehr.util.configuration;

import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.env.Environment;
import se.cambio.openehr.util.UserConfigurationManager;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

@Configuration
@PropertySource(value = {"file:conf/UserConfig.properties", "file:${user.home}/.gdleditor/UserConfig.properties"}, ignoreResourceNotFound = true)
public class UserConfiguration {

    @Autowired
    private Environment environment;

    private static UserConfigurationManager userConfigurationManager = new UserConfigurationManager();

    @Bean
    public UserConfigurationManager userConfigurationManager() {
        String language = environment.getProperty(UserConfigurationManager.LANGUAGE, "en");
        userConfigurationManager.setLanguage(language);
        String country = environment.getProperty(UserConfigurationManager.COUNTRY, "EN");
        userConfigurationManager.setCountry(country);
        Date currentDateTime = getCurrentDateTime();
        userConfigurationManager.setCurrentDateTime(currentDateTime);
        String activeEngine = environment.getProperty(UserConfigurationManager.ACTIVE_RULE_ENGINE, "rule-drools-engine");
        userConfigurationManager.setActiveRuleEngine(activeEngine);
        Map<String, String> pathMap = new HashMap<>();
        populatePathMap(pathMap, UserConfigurationManager.ARCHETYPES_FOLDER, "archetypes");
        populatePathMap(pathMap, UserConfigurationManager.TEMPLATES_FOLDER, "templates");
        populatePathMap(pathMap, UserConfigurationManager.TERMINOLOGIES_FOLDER, "terminologies");
        populatePathMap(pathMap, UserConfigurationManager.GUIDELINES_FOLDER, "guidelines");
        populatePathMap(pathMap, UserConfigurationManager.DOCUMENTS_FOLDER, "docs");
        userConfigurationManager.setPathMap(pathMap);
        return userConfigurationManager;
    }

    private Date getCurrentDateTime() {
        Date currentDateTime = null;
        String currentDateStr = environment.getProperty(UserConfigurationManager.CURRENT_DATE_TIME, "");
        if (!currentDateStr.isEmpty()) {
            currentDateTime = new DateTime(currentDateStr).toDate();
        }
        return currentDateTime;
    }

    private void populatePathMap(Map<String, String> pathMap, String folderProperty, String defaultValue) {
        pathMap.put(folderProperty, environment.getProperty(folderProperty, defaultValue));
    }


    public static UserConfigurationManager getInstanceUserConfigurationManager() {
        return userConfigurationManager;
    }
}
