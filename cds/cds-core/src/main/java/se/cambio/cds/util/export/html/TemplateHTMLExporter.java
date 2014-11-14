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

public class TemplateHTMLExporter extends ClinicalModelHTMLExporter<Archetype> {

    private final ArchetypeDefinitionHTMLRenderer archetypeDefinitionHTMLRenderer;
    private String templateId;

    public TemplateHTMLExporter(ArchetypeManager archetypeManager) {
        super(archetypeManager);
        this.archetypeDefinitionHTMLRenderer = new ArchetypeDefinitionHTMLRenderer(archetypeManager);
    }

    public void setTemplateId(String templateId) {
        this.templateId = templateId;
    }

    @Override
    public Map<String, Object> getEntityObjectsMap() throws InternalErrorException, InstanceNotFoundException {
        ArchetypeEntityObjectMapGenerator archetypeEntityObjectMapGenerator =
                new ArchetypeEntityObjectMapGenerator(getEntity(), getLanguage(), archetypeDefinitionHTMLRenderer);
        Map<String, Object> objectMap = archetypeEntityObjectMapGenerator.getEntityObjectsMap();
        objectMap.put("templateId", templateId);
        return objectMap;
    }

    @Override
    public Map<String, String> getEntityTextMap() {
        Map<String, String> textsMap = new HashMap<String, String>();
        addText(textsMap, "TemplateDefinition");
        return textsMap;
    }

    @Override
    public InputStream getInputStreamTemplate() {
        return TemplateHTMLExporter.class.getClassLoader().getResourceAsStream("template.ftl");
    }

    public String getIconPath() {
        return archetypeDefinitionHTMLRenderer.getIconPath();
    }

    public void setIconPath(String iconPath) {
        archetypeDefinitionHTMLRenderer.setIconPath(iconPath);
    }
}
