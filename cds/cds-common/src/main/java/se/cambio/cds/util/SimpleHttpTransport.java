package se.cambio.cds.util;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;

import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.MissingConfigurationParameterException;

/**
 * Uses java.net.HttpURLConnection to send http message.
 *
 * @author markopi
 * @since 22.10.2012
 */
public class SimpleHttpTransport {

    private static int CONNECTION_TIMEOUT = 5000;
    /**
     * If you use this constructor, you must also call {@link #setThinkhubBaseUrl}
     */
    public SimpleHttpTransport() {
    }

    public static void send(String message) throws InternalErrorException {
	try {
	    StringBuilder urlBuilder = new StringBuilder(getThinkhubBaseUrl()).append("/receive.json");
	    urlBuilder.append("?type=").append("Event");
	    //urlBuilder.append("&data=").append(URLEncoder.encode(serializeEntry(entry), "utf-8"));

	    byte[] content = message.getBytes("utf-8");

	    URL url = new URL(urlBuilder.toString());
	    HttpURLConnection con = (HttpURLConnection) url.openConnection();
	    con.setRequestMethod("POST");
	    con.setConnectTimeout(CONNECTION_TIMEOUT);
	    con.setDoInput(true);
	    con.setDoOutput(true);
	    con.addRequestProperty("Content-type", "application/json");
	    con.addRequestProperty("Content-Length", Integer.toString(content.length));

	    con.getOutputStream().write(content);
	    con.getOutputStream().flush();


	    if (con.getResponseCode() != HttpURLConnection.HTTP_OK) {
		throw new IOException(
			"URL '" + url + "' returned an invalid response code (" + con.getResponseCode() + ") : " +
				con.getResponseMessage());
	    }
	}catch(Exception e){
	    throw new InternalErrorException(e);
	}
    }

    private static String getThinkhubBaseUrl() throws MissingConfigurationParameterException{
	return EHRConnectorConfigurationParametersManager.getParameter(EHRConnectorConfigurationParametersManager.REMOTE_LOGGER_URL);
    }

}
