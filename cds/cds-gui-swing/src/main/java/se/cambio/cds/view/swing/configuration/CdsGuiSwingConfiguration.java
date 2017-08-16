package se.cambio.cds.view.swing.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import se.cambio.cds.controller.session.data.ArchetypeReferencesManager;
import se.cambio.cds.view.swing.DvSwingManager;
import se.cambio.openehr.configuration.OpenEhrSwingConfiguration;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.view.util.DVPanelFactory;

@Configuration
@Import(OpenEhrSwingConfiguration.class)
public class CdsGuiSwingConfiguration {
    @Bean
    DvSwingManager dvSwingManager(
            ArchetypeManager archetypeManager,
            ArchetypeReferencesManager archetypeReferencesManager,
            DVPanelFactory dvPanelFactory) {
        return new DvSwingManager(archetypeManager, archetypeReferencesManager, dvPanelFactory);
    }
}
