package se.cambio.openehr.controller.session.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import se.cambio.cm.configuration.TerminologyServiceConfiguration;
import se.cambio.cm.controller.terminology.TerminologyService;
import se.cambio.cm.model.facade.administration.delegate.ClinicalModelsService;
import se.cambio.cm.model.facade.configuration.ClinicalModelsConfiguration;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.controller.session.data.Terminologies;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.configuration.UserConfiguration;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@Configuration
@Import({ClinicalModelsConfiguration.class, TerminologyServiceConfiguration.class, UserConfiguration.class})
public class ClinicalModelsCacheConfiguration {

    @Bean
    ExecutorService executorService() {
        return Executors.newCachedThreadPool();
    }

    @Bean
    public ArchetypeManager archetypeManager(
            ClinicalModelsService clinicalModelsService,
            TerminologyService terminologyService,
            UserConfigurationManager userConfigurationManager,
            ExecutorService executorService) {
        return new ArchetypeManager(clinicalModelsService, terminologyService, userConfigurationManager, executorService);
    }

    @Bean
    Terminologies terminologies(ClinicalModelsService clinicalModelsService) {
        return new Terminologies(clinicalModelsService);
    }
}