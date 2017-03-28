package se.cambio.openehr.controller.session.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import se.cambio.cm.configuration.TerminologyServiceConfiguration;
import se.cambio.cm.controller.terminology.TerminologyService;
import se.cambio.cm.model.facade.administration.delegate.CMAdministrationFacadeDelegate;
import se.cambio.cm.model.facade.configuration.ClinicalModelsAdministrationConfiguration;
import se.cambio.openehr.controller.session.data.ArchetypeManager;

@Configuration
@Import({ClinicalModelsAdministrationConfiguration.class, TerminologyServiceConfiguration.class})
public class ClinicalModelsCacheConfiguration {

    @Bean
    ArchetypeManager archetypeManager(CMAdministrationFacadeDelegate cmAdministrationFacadeDelegate, TerminologyService terminologyService) {
        return new ArchetypeManager(cmAdministrationFacadeDelegate, terminologyService);
    }
}
