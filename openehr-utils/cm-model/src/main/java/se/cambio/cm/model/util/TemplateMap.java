package se.cambio.cm.model.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class TemplateMap {
    private final String archetypeId;
    private final String templateId;
    private Map<String, TemplateElementMap> elementMaps;

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
        if (elementMaps == null) {
            elementMaps = new HashMap<String, TemplateElementMap>();
        }
        return Collections.unmodifiableMap(elementMaps);
    }
}
