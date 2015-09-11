package se.cambio.cm.configuration;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.context.annotation.PropertySources;

@Configuration
@ComponentScan({"se.cambio.cm.model", "se.cambio.openehr.model"})
@PropertySources({
        @PropertySource("classpath:cds-config-default.properties"),
        @PropertySource(value = "file:${CDS_CONFIG_DIR:/opt/cds-config}/default-cds-config.properties", ignoreResourceNotFound = true)
})
public class CmServiceConfiguration {
}