package se.cambio.openehr.controller.terminology.ts;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import se.cambio.cm.model.configuration.CmPersistenceConfig;

@Configuration
@Import(CmPersistenceConfig.class)
@ComponentScan({"se.cambio.cm.model"})
public class TerminologyTestConfig {
}
