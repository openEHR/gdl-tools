package se.cambio.cds;
import org.apache.log4j.Logger;

import se.cambio.cds.util.EHRConnectorConfigurationParametersManager;
import se.cambio.cds.util.LogEventVO;


public class LogTest {

    public static void main(String[] args) throws Exception{
	EHRConnectorConfigurationParametersManager.getParameter(EHRConnectorConfigurationParametersManager.REMOTE_LOGGER_URL);
	for (int i = 0; i < 1; i++) {
	    LogEventVO logEventVO = new LogEventVO("Test Context", "Test", 20L);
	    Logger.getLogger(LogTest.class).info(logEventVO);
	    Thread.sleep(200);
	}
    }
    
}
