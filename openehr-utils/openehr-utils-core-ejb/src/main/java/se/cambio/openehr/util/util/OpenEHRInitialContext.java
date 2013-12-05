package se.cambio.openehr.util.util;

import org.apache.log4j.Logger;
import org.jboss.ejb.client.ContextSelector;
import org.jboss.ejb.client.EJBClientConfiguration;
import org.jboss.ejb.client.EJBClientContext;
import org.jboss.ejb.client.PropertiesBasedEJBClientConfiguration;
import org.jboss.ejb.client.remoting.ConfigBasedEJBClientContextSelector;
import se.cambio.openehr.util.exceptions.MissingConfigurationParameterException;
import se.cambio.openehr.util.misc.OpenEHRConfigurationParametersManager;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import java.util.Properties;

/**
 * User: Iago.Corbal
 * Date: 2013-10-28
 * Time: 13:00
 */
public class OpenEHRInitialContext {


    public static String getOpenEHRServerHost() throws MissingConfigurationParameterException {
        return OpenEHRConfigurationParametersManager.getParameter(OpenEHRConfigurationParametersManager.OPENEHR_SERVER_HOST);
    }

    public static String getOpenEHRServerPort() throws MissingConfigurationParameterException {
        return OpenEHRConfigurationParametersManager.getParameter(OpenEHRConfigurationParametersManager.OPENEHR_SERVER_PORT);
    }

    public static String getOpenEHRServerLogin() throws MissingConfigurationParameterException {
        return OpenEHRConfigurationParametersManager.getParameter(OpenEHRConfigurationParametersManager.OPENEHR_SERVER_USER_LOGIN);
    }

    public static String getOpenEHRServerPassword() throws MissingConfigurationParameterException {
        return OpenEHRConfigurationParametersManager.getParameter(OpenEHRConfigurationParametersManager.OPENEHR_SERVER_USER_PASSWD);
    }

    public static InitialContext getInitialContext() throws NamingException {
        String host = null;
        String port = null;
        String login = null;
        String password = null;
        try {
            host = getOpenEHRServerHost();
            port = getOpenEHRServerPort();
            login = getOpenEHRServerLogin();
            password = getOpenEHRServerPassword();
        } catch (MissingConfigurationParameterException e) {
            Logger.getLogger(OpenEHRInitialContext.class).info("No URL for OpenEHR found on OpenEHR Config using default.");
        }
        return getInitialContext(host, port, login, password);
    }

    public static InitialContext getInitialContext(String host, String port, String login, String password) throws NamingException {
        if (host!=null && port!=null){
            Properties clientProp = new Properties();
            clientProp.put("remote.connectionprovider.create.options.org.xnio.Options.SSL_ENABLED", "false");
            clientProp.put("remote.connections", "default");
            clientProp.put("remote.connection.default.host", host); // comes from JVM argument
            clientProp.put("remote.connection.default.port", port); // comes from JVM argument
            clientProp.put("remote.connection.default.connect.options.org.xnio.Options.SASL_POLICY_NOANONYMOUS", "false");
            if (login!=null && password!=null){
                clientProp.put("remote.connection.default.username", login);
                clientProp.put("remote.connection.default.password", password);
            }
            EJBClientConfiguration cc = new PropertiesBasedEJBClientConfiguration(clientProp);
            ContextSelector<EJBClientContext> selector = new ConfigBasedEJBClientContextSelector(cc);
            EJBClientContext.setSelector(selector);
        }
        Properties props = new Properties();
        props.put(InitialContext.URL_PKG_PREFIXES, "org.jboss.ejb.client.naming");
        return new InitialContext(props);
    }
}
