package se.cambio.cds.configuration;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import se.cambio.cm.configuration.TerminologyServiceConfiguration;

@Configuration
@ComponentScan({"se.cambio.cds.controller", "se.cambio.cds.util"})
@Import(TerminologyServiceConfiguration.class)
public class CdsCoreConfiguration {
}
