package se.cambio.cm.model.util;

import java.util.Collections;
import java.util.Map;

public class TemplateMap {
    private final String archetypeId;
    private final String templateId;
    private final Map<String, TemplateElementMap> elementMaps;

    public TemplateMap(String archetypeId, String templateId, Map<String, TemplateElementMap> elementMaps) {
        this.archetypeId = archetypeId;
        this.templateId = templateId;
        this.elementMaps = elementMaps;
    }

    public String getArchetypeId() {
        return archetypeId;
    }

    public String getTemplateId() {
        return templateId;
    }

    public Map<String, TemplateElementMap> getElementMaps() {
        return Collections.unmodifiableMap(elementMaps);
    }
}
