package se.cambio.cm.configuration;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.context.annotation.PropertySources;

@Configuration
@ComponentScan({"se.cambio.cm.model", "se.cambio.openehr.model"})
@PropertySource("classpath:cds-config-default.properties")
public class CmServiceConfiguration {
}