package se.cambio.cm.model.facade.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import se.cambio.cm.configuration.CmPersistenceConfig;
import se.cambio.cm.model.facade.administration.delegate.ClinicalModelsService;
import se.cambio.cm.model.facade.administration.plain.PlainClinicalModelsService;
import se.cambio.cm.model.util.CMElementDAOFactory;

@Configuration
@Import(CmPersistenceConfig.class)
public class ClinicalModelsConfiguration {

    @Bean
    ClinicalModelsService clinicalModelsService(CMElementDAOFactory cmElementDAOFactory) {
        return new PlainClinicalModelsService(cmElementDAOFactory);
    }
}
