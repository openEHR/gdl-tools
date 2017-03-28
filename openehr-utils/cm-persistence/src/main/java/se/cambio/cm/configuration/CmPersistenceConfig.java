package se.cambio.cm.configuration;

import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import se.cambio.cm.model.util.CMElementDAOFactory;

@Configuration
@ComponentScan({"se.cambio.cm.model"})
public class CmPersistenceConfig {

    @Bean
    CMElementDAOFactory cmElementDAOFactory(ApplicationContext applicationContext) {
        return new CMElementDAOFactory(applicationContext);
    }

}