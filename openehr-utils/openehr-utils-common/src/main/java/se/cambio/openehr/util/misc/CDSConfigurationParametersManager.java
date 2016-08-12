package se.cambio.openehr.util.misc;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.context.annotation.PropertySources;
import org.springframework.core.env.Environment;

@Configuration
@PropertySources({
        @PropertySource(value = "classpath:default-cds-config.properties", ignoreResourceNotFound = true),
        @PropertySource(value = "file:conf/cds-config.properties", ignoreResourceNotFound = true),
})
public class CDSConfigurationParametersManager {
    private static final String KM_SERVER_URL = "km-server.url";
    private static final String KM_SERVER_USER_LOGIN = "km-server.login";
    private static final String KM_SERVER_USER_PASSWD = "km-server.password";
    private static final String CDS_EXECUTION_TIMEOUT = "cds-execution.timeout";
    private static final String DB_URL = "km-server.db.url";
    private static final String DB_USER = "km-server.db.user";
    private static final String DB_PASSWORD = "km-server.db.password";

    @Autowired
    Environment environment;

    public CDSConfigurationParametersManager() {
    }

    public String getKmUrl() {
        return environment.getProperty(KM_SERVER_URL, "http://localhost:8080/km");
    }

    public String getKmUser() {
        return environment.getProperty(KM_SERVER_USER_LOGIN, "km");
    }

    public String getKmPassword() {
        return environment.getProperty(KM_SERVER_USER_PASSWD, "km");
    }

    public Long getCdsExecutionTimeOut() {
        return environment.getProperty(CDS_EXECUTION_TIMEOUT, Long.class, 600000L);
    }

    public String getDbUrl() {
        return environment.getProperty(DB_URL);
    }

    public String getDbUser() {
        return environment.getProperty(DB_USER);
    }

    public String getDbPassword() {
        return environment.getProperty(DB_PASSWORD);
    }

}