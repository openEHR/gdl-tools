package se.cambio.cds.model.integration;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

public class EHRDataInstance implements Serializable {
    private static final long serialVersionUID = 20140404L;
    private String templateId;
    private String archetypeId;
    private Map<String, String> elementDataMap;

    public EHRDataInstance(String templateId, String archetypeId) {
        this.templateId = templateId;
        this.archetypeId = archetypeId;
        this.elementDataMap = new HashMap<String, String>();
    }

    public String getTemplateId() {
        return templateId;
    }

    public void setTemplateId(String templateId) {
        this.templateId = templateId;
    }

    public String getArchetypeId() {
        return archetypeId;
    }

    public void setArchetypeId(String archetypeId) {
        this.archetypeId = archetypeId;
    }

    public Map<String, String> getElementDataMap() {
        return elementDataMap;
    }

    public void setElementDataMap(Map<String, String> elementDataMap) {
        this.elementDataMap = elementDataMap;
    }

    public void addElement(String elementId, String elementValue){
        this.elementDataMap.put(elementId, elementValue);
    }
}
