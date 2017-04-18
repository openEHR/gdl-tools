package se.cambio.cm.configuration;

import org.openehr.rm.support.terminology.TerminologyService;
import org.springframework.context.annotation.*;

@Configuration
@ComponentScan({"se.cambio.cm.model", "se.cambio.openehr.model"})
@PropertySource("classpath:cds-config-default.properties")
public class CmServiceConfiguration {

}