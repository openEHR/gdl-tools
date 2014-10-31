package se.cambio.cm.model.util;

import java.util.Collections;
import java.util.Map;

public class TemplateMap {
    public final String archetypeId;
    public final String templateId;
    public final Map<String, TemplateElementMap> elementMaps;

    public TemplateMap(String archetypeId, String templateId, Map<String, TemplateElementMap> elementMaps) {
        this.archetypeId = archetypeId;
        this.templateId = templateId;
        this.elementMaps = elementMaps;
    }

    public Map<String, TemplateElementMap> getElementMaps() {
        return Collections.unmodifiableMap(elementMaps);
    }

    public static class TemplateElementMap {
        public final String dataType;
        public final String path;
        public final String elementMapId;

        public TemplateElementMap(String dataType, String path, String elementMapId) {
            this.dataType = dataType;
            this.path = path;
            this.elementMapId = elementMapId;
        }

        public String getElementMapId() {
            return elementMapId;
        }
    }
}
