package se.cambio.cds.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import se.cambio.cds.controller.execution.DroolsExecutionManager;
import se.cambio.cds.gdl.converters.drools.DroolsGuideExportPlugin;
import se.cambio.cds.model.facade.execution.drools.DroolsRuleEngineFacadeDelegate;

@Configuration
@Profile("rule-drools-engine")
public class DroolsConfiguration {

    @Bean
    public DroolsExecutionManager droolsExecutionManager() {
        return new DroolsExecutionManager();
    }

    @Bean
    public DroolsGuideExportPlugin droolsGuideExportPlugin() {
        return new DroolsGuideExportPlugin();
    }

    @Bean
    public DroolsRuleEngineFacadeDelegate droolsRuleEngineFacadeDelegate(DroolsExecutionManager droolsExecutionManager) {
        return new DroolsRuleEngineFacadeDelegate(droolsExecutionManager);
    }
}
