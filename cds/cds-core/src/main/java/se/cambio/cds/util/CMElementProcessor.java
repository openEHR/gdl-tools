package se.cambio.cds.util;

import se.cambio.cds.controller.session.data.Guides;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.cm.model.template.dto.TemplateDTO;
import se.cambio.cm.model.util.CMElement;
import se.cambio.openehr.controller.session.data.ArchetypeManager;

import java.util.Collections;

public class CMElementProcessor<E extends CMElement> {
    ArchetypeManager archetypeManager;
    Guides guides;

    public CMElementProcessor() {
    }

    public void process(E cmElement) {
        if (cmElement instanceof ArchetypeDTO) {
            getArchetypeManager().getArchetypes().processArchetype((ArchetypeDTO)cmElement);
        } else if (cmElement instanceof TemplateDTO) {
            getArchetypeManager().getTemplates().processTemplate((TemplateDTO) cmElement);
        } else if (cmElement instanceof GuideDTO) {
           getGuides().processGuides(Collections.singleton((GuideDTO)cmElement));
        }
    }

    public ArchetypeManager getArchetypeManager() {
        if (archetypeManager == null) {
            archetypeManager = new ArchetypeManager();
        }
        return archetypeManager;
    }

    public Guides getGuides() {
        if (guides == null) {
            guides = new Guides();
        }
        return guides;
    }
}
