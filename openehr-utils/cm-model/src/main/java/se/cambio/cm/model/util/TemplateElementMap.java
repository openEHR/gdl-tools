package se.cambio.cm.model.util;

import java.util.HashMap;
import java.util.Map;

public class TemplateElementMap {
    private final String dataType;
    private final String path;
    private final String elementMapId;
    private Map<String, TemplateAttributeMap> attributeMaps;

    public TemplateElementMap(String dataType, String path, String elementMapId, Map<String, TemplateAttributeMap> attributeMap) {
        this.dataType = dataType;
        this.path = path;
        this.elementMapId = elementMapId;
        this.attributeMaps = attributeMap;
    }

    public String getDataType() {
        return dataType;
    }

    public String getPath() {
        return path;
    }

    public String getElementMapId() {
        return elementMapId;
    }

    public Map<String, TemplateAttributeMap> getAttributeMaps() {
        if (attributeMaps == null) {
            attributeMaps = new HashMap<String, TemplateAttributeMap>();
        }
        return attributeMaps;
    }
}