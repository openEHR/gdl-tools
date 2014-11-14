package se.cambio.cm.model.archetype.vo;

public class ArchetypeElementVOBuilder {
    private String name;
    private String description;
    private String type;
    private String idArchetype;
    private String idTemplate;
    private String path;

    public ArchetypeElementVOBuilder setName(String name) {
        this.name = name;
        return this;
    }

    public ArchetypeElementVOBuilder setDescription(String description) {
        this.description = description;
        return this;
    }

    public ArchetypeElementVOBuilder setType(String type) {
        this.type = type;
        return this;
    }

    public ArchetypeElementVOBuilder setIdArchetype(String idArchetype) {
        this.idArchetype = idArchetype;
        return this;
    }

    public ArchetypeElementVOBuilder setIdTemplate(String idTemplate) {
        this.idTemplate = idTemplate;
        return this;
    }

    public ArchetypeElementVOBuilder setPath(String path) {
        this.path = path;
        return this;
    }

    public ArchetypeElementVO createArchetypeElementVO() {
        return new ArchetypeElementVO(name, description, type, idArchetype, idTemplate, path);
    }
}