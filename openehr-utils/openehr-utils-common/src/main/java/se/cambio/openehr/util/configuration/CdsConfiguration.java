package se.cambio.openehr.util.configuration;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import se.cambio.openehr.util.UserConfigurationManager;

@Configuration
@ComponentScan({"se.cambio"})
@Import(UserConfigurationManager.class)
public class CdsConfiguration {



}
