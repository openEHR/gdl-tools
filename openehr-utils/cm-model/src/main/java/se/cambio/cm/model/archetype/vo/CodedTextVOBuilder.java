package se.cambio.cm.model.archetype.vo;

public class CodedTextVOBuilder {
    private String name;
    private String description;
    private String type;
    private String idArchetype;
    private String idTemplate;
    private String path;
    private String terminology;
    private String code;

    public CodedTextVOBuilder setName(String name) {
        this.name = name;
        return this;
    }

    public CodedTextVOBuilder setDescription(String description) {
        this.description = description;
        return this;
    }

    public CodedTextVOBuilder setType(String type) {
        this.type = type;
        return this;
    }

    public CodedTextVOBuilder setIdArchetype(String idArchetype) {
        this.idArchetype = idArchetype;
        return this;
    }

    public CodedTextVOBuilder setIdTemplate(String idTemplate) {
        this.idTemplate = idTemplate;
        return this;
    }

    public CodedTextVOBuilder setPath(String path) {
        this.path = path;
        return this;
    }

    public CodedTextVOBuilder setTerminology(String terminology) {
        this.terminology = terminology;
        return this;
    }

    public CodedTextVOBuilder setCode(String code) {
        this.code = code;
        return this;
    }

    public CodedTextVO createCodedTextVO() {
        return new CodedTextVO(name, description, type, idArchetype, idTemplate, path, terminology, code);
    }
}