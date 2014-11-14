package se.cambio.cds.util.export.html;

import org.openehr.am.archetype.Archetype;
import se.cambio.cds.util.export.html.util.ArchetypeDefinitionHTMLRenderer;
import se.cambio.cds.util.export.html.util.ArchetypeEntityObjectMapGenerator;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

public class ArchetypeHTMLExporter extends ClinicalModelHTMLExporter<Archetype> {

    private final ArchetypeDefinitionHTMLRenderer archetypeDefinitionHTMLRenderer;

    public ArchetypeHTMLExporter(ArchetypeManager archetypeManager) {
        super(archetypeManager);
        this.archetypeDefinitionHTMLRenderer = new ArchetypeDefinitionHTMLRenderer(archetypeManager);
    }


    @Override
    public Map<String, Object> getEntityObjectsMap() throws InternalErrorException, InstanceNotFoundException {
        ArchetypeEntityObjectMapGenerator archetypeEntityObjectMapGenerator =
                new ArchetypeEntityObjectMapGenerator(getEntity(), getLanguage(), archetypeDefinitionHTMLRenderer);
        return archetypeEntityObjectMapGenerator.getEntityObjectsMap();
    }

    @Override
    public Map<String, String> getEntityTextMap() {
        Map<String, String> textsMap = new HashMap<String, String>();
        addText(textsMap, "ArchetypeDetails");
        addText(textsMap, "ArchetypeDefinition");
        return textsMap;
    }

    @Override
    public InputStream getInputStreamTemplate() {
        return ArchetypeHTMLExporter.class.getClassLoader().getResourceAsStream("archetype.ftl");
    }

    public String getIconPath() {
        return archetypeDefinitionHTMLRenderer.getIconPath();
    }

    public void setIconPath(String iconPath) {
        archetypeDefinitionHTMLRenderer.setIconPath(iconPath);
    }
}
