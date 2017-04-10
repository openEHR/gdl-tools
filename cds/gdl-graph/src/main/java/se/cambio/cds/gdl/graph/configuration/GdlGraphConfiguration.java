package se.cambio.cds.gdl.graph.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import se.cambio.cds.controller.session.data.ArchetypeReferencesManager;
import se.cambio.cds.gdl.graph.view.panel.GdlGraphManager;
import se.cambio.cds.util.ElementInstanceCollectionManager;
import se.cambio.openehr.controller.session.data.ArchetypeManager;

@Configuration
public class GdlGraphConfiguration {

    @Bean
    public GdlGraphManager gdlGraphPluginPanel(
            ArchetypeManager archetypeManager,
            ArchetypeReferencesManager archetypeReferencesManager,
            ElementInstanceCollectionManager elementInstanceCollectionManager) {
        return new GdlGraphManager(
                archetypeManager,
                archetypeReferencesManager,
                elementInstanceCollectionManager);
    }
}
