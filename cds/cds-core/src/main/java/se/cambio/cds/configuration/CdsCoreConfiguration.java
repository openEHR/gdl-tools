package se.cambio.cds.configuration;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"se.cambio.cds.controller", "se.cambio.cds.util"})
public class CdsCoreConfiguration {
}
