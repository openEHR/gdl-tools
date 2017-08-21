package se.cambio.cds.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.context.annotation.Profile;
import se.cambio.cds.controller.execution.DroolsExecutionManager;
import se.cambio.cds.gdl.converters.drools.DroolsGuideExportPlugin;
import se.cambio.cds.model.facade.execution.drools.DroolsRuleEngineService;
import se.cambio.openehr.controller.session.configuration.ClinicalModelsCacheConfiguration;
import se.cambio.openehr.controller.session.data.ArchetypeManager;

@Configuration
@Import(ClinicalModelsCacheConfiguration.class)
@Profile("rule-drools-engine")
public class DroolsConfiguration {

    @Bean
    public DroolsExecutionManager droolsExecutionManager(ArchetypeManager archetypeManager) {
        return new DroolsExecutionManager(archetypeManager);
    }

    @Bean
    public DroolsGuideExportPlugin droolsGuideExportPlugin(ArchetypeManager archetypeManager) {
        return new DroolsGuideExportPlugin(archetypeManager);
    }

    @Bean
    public DroolsRuleEngineService droolsRuleEngineService(DroolsExecutionManager droolsExecutionManager) {
        return droolsExecutionManager.getDroolsRuleEngineService();
    }
}
