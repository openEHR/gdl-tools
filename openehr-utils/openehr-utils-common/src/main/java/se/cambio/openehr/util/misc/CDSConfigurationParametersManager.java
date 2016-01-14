package se.cambio.openehr.util.misc;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.context.annotation.PropertySources;
import org.springframework.core.env.Environment;

@Configuration
@PropertySources({
        @PropertySource(value = "classpath:default-cds-config.properties", ignoreResourceNotFound = true),
        @PropertySource(value = "file:${CDS_CONFIG_DIR:/opt/cds-config}/cds-config.properties", ignoreResourceNotFound = true),
        @PropertySource(value = "file:conf/cds-config.properties", ignoreResourceNotFound = true),
})
public class CDSConfigurationParametersManager {
    private static final String KM_SERVER_HOST = "km-server.host";
    private static final String KM_SERVER_PORT = "km-server.port";
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

    public String getKmHost() {
        return environment.getProperty(KM_SERVER_HOST, String.class, "localhost");
    }

    public Integer getKmPort() {
        return environment.getProperty(KM_SERVER_PORT, Integer.class, 8080);
    }

    public String getKmUser() {
        return environment.getProperty(KM_SERVER_USER_LOGIN, String.class, "km");
    }

    public String getKmPassword() {
        return environment.getProperty(KM_SERVER_USER_PASSWD, String.class, "km");
    }

    public Long getCdsExecutionTimeOut() {
        return environment.getProperty(CDS_EXECUTION_TIMEOUT, Long.class, 10000L);
    }

    public String getDbUrl() {
        return environment.getProperty(DB_URL, String.class);
    }

    public String getDbUser() {
        return environment.getProperty(DB_USER, String.class);
    }

    public String getDbPassword() {
        return environment.getProperty(DB_PASSWORD, String.class);
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