package se.cambio.cm.model.archetype.vo;

public class OrdinalVOBuilder {
    private String name;
    private String description;
    private String type;
    private String idArchetype;
    private String idTemplate;
    private String path;
    private Integer value;
    private String terminology;
    private String code;

    public OrdinalVOBuilder setName(String name) {
        this.name = name;
        return this;
    }

    public OrdinalVOBuilder setDescription(String description) {
        this.description = description;
        return this;
    }

    public OrdinalVOBuilder setType(String type) {
        this.type = type;
        return this;
    }

    public OrdinalVOBuilder setIdArchetype(String idArchetype) {
        this.idArchetype = idArchetype;
        return this;
    }

    public OrdinalVOBuilder setIdTemplate(String idTemplate) {
        this.idTemplate = idTemplate;
        return this;
    }

    public OrdinalVOBuilder setPath(String path) {
        this.path = path;
        return this;
    }

    public OrdinalVOBuilder setValue(Integer value) {
        this.value = value;
        return this;
    }

    public OrdinalVOBuilder setTerminology(String terminology) {
        this.terminology = terminology;
        return this;
    }

    public OrdinalVOBuilder setCode(String code) {
        this.code = code;
        return this;
    }

    public OrdinalVO createOrdinalVO() {
        return new OrdinalVO(name, description, type, idArchetype, idTemplate, path, value, terminology, code);
    }
}