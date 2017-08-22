package se.cambio.openehr.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import se.cambio.cm.controller.terminology.TerminologyService;
import se.cambio.cm.model.facade.administration.delegate.ClinicalModelsService;
import se.cambio.openehr.controller.session.configuration.ClinicalModelsCacheConfiguration;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.TerminologyDialogManager;
import se.cambio.openehr.util.TerminologyNodeManager;
import se.cambio.openehr.view.util.DVPanelFactory;
import se.cambio.openehr.view.util.ImportManager;
import se.cambio.openehr.view.util.WindowManager;

@Configuration
@Import(ClinicalModelsCacheConfiguration.class)
public class OpenEhrSwingConfiguration {

    @Bean
    TerminologyNodeManager terminologyNodeManager(TerminologyService terminologyService) {
        return new TerminologyNodeManager(terminologyService);
    }

    @Bean
    TerminologyDialogManager terminologyDialogs(WindowManager windowManager, TerminologyNodeManager terminologyNodeManager) {
        return new TerminologyDialogManager(windowManager, terminologyNodeManager);
    }

    @Bean
    DVPanelFactory dvPanelFactory(WindowManager windowManager, ArchetypeManager archetypeManager, TerminologyDialogManager terminologyDialogManager) {
        return new DVPanelFactory(archetypeManager, terminologyDialogManager, windowManager);
    }

    @Bean
    ImportManager importManager(
            ClinicalModelsService clinicalModelsService,
            ArchetypeManager archetypeManager) {
        return new ImportManager(archetypeManager, clinicalModelsService);
    }

    @Bean
    WindowManager windowManager() {
        return new WindowManager();
    }
}
