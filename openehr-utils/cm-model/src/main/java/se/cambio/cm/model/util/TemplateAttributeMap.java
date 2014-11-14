package se.cambio.cm.model.util;

public class TemplateAttributeMap {
    private String attributeId;
    private String terminologyId;
    private String code;
    private Integer value;
    private String text;

    public TemplateAttributeMap(String attributeId, String terminologyId, String code, Integer value, String text) {
        this.attributeId = attributeId;
        this.terminologyId = terminologyId;
        this.code = code;
        this.value = value;
        this.text = text;
    }

    public String getAttributeId() {
        return attributeId;
    }

    public String getTerminologyId() {
        return terminologyId;
    }

    public String getCode() {
        return code;
    }

    public Integer getValue() {
        return value;
    }

    public String getText() {
        return text;
    }
}
