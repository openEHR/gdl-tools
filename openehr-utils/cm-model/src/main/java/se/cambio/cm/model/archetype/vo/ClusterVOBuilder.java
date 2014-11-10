package se.cambio.cm.model.archetype.vo;

public class ClusterVOBuilder {
    private String name;
    private String description;
    private String type;
    private String idArchetype;
    private String idTemplate;
    private String path;

    public ClusterVOBuilder setName(String name) {
        this.name = name;
        return this;
    }

    public ClusterVOBuilder setDescription(String description) {
        this.description = description;
        return this;
    }

    public ClusterVOBuilder setType(String type) {
        this.type = type;
        return this;
    }

    public ClusterVOBuilder setIdArchetype(String idArchetype) {
        this.idArchetype = idArchetype;
        return this;
    }

    public ClusterVOBuilder setIdTemplate(String idTemplate) {
        this.idTemplate = idTemplate;
        return this;
    }

    public ClusterVOBuilder setPath(String path) {
        this.path = path;
        return this;
    }

    public ClusterVO createClusterVO() {
        return new ClusterVO(name, description, type, idArchetype, idTemplate, path);
    }
}