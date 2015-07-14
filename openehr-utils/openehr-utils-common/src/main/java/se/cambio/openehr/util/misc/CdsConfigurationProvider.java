package se.cambio.openehr.util.misc;

import se.cambio.openehr.util.BeanProvider;

public class CdsConfigurationProvider {
    private static CDSConfigurationParametersManager cdsConfigManager;

    public static CDSConfigurationParametersManager getCdsConfiguration() {
        if (cdsConfigManager == null) {
            cdsConfigManager = BeanProvider.getBean(CDSConfigurationParametersManager.class);
        }
        return cdsConfigManager;
    }
}
