package se.cambio.cds.model.facade.ehr.dummy.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import se.cambio.cds.model.facade.ehr.delegate.EhrService;
import se.cambio.cds.model.facade.ehr.dummy.DummyEhrService;

@Configuration
@Profile("ehr-dummy-service")
public class EhrDummyServiceConfiguration {
    @Bean
    EhrService ehrService() {
        return new DummyEhrService();
    }
}
