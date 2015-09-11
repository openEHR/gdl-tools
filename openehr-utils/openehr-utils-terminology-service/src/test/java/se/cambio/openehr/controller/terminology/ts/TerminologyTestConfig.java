package se.cambio.openehr.controller.terminology.ts;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import se.cambio.cm.configuration.CmServiceConfiguration;
import se.cambio.cm.configuration.TerminologyServiceConfiguration;
import se.cambio.cm.model.configuration.CmPersistenceConfig;

@Configuration
@Import({CmPersistenceConfig.class, CmServiceConfiguration.class, TerminologyServiceConfiguration.class})
public class TerminologyTestConfig {
}
