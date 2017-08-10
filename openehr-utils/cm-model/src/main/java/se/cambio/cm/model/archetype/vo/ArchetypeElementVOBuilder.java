package se.cambio.cm.model.archetype.vo;

public class ArchetypeElementVOBuilder {
    private String name;
    private String description;
    private String type;
    private String idArchetype;
    private String idTemplate;
    private String path;
    private Integer lowerCardinality;
    private Integer upperCardinality;

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

    public ArchetypeElementVOBuilder setLowerCardinality(Integer lowerCardinality) {
        this.lowerCardinality = lowerCardinality;
        return this;
    }

    public ArchetypeElementVOBuilder setUpperCardinality(Integer upperCardinality) {
        this.upperCardinality = upperCardinality;
        return this;
    }

    public ArchetypeElementVO createArchetypeElementVO() {
        ArchetypeElementVO archetypeElementVO = new ArchetypeElementVO(name, description, type, idArchetype, idTemplate, path);
        archetypeElementVO.setLowerCardinality(lowerCardinality);
        archetypeElementVO.setUpperCardinality(upperCardinality);
        return archetypeElementVO;
    }
}