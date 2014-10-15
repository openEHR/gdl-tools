package se.cambio.openehr.model.template.dto;

public class TemplateDTOBuilder {
    private String templateId;
    private String arcehtypeId;
    private String name;
    private String description;
    private String entryType;
    private String archetype;
    private byte[] aom;
    private byte[] aobcVO;

    public TemplateDTOBuilder setTemplateId(String templateId) {
        this.templateId = templateId;
        return this;
    }

    public TemplateDTOBuilder setArcehtypeId(String arcehtypeId) {
        this.arcehtypeId = arcehtypeId;
        return this;
    }

    public TemplateDTOBuilder setName(String name) {
        this.name = name;
        return this;
    }

    public TemplateDTOBuilder setDescription(String description) {
        this.description = description;
        return this;
    }

    public TemplateDTOBuilder setEntryType(String entryType) {
        this.entryType = entryType;
        return this;
    }

    public TemplateDTOBuilder setArchetype(String archetype) {
        this.archetype = archetype;
        return this;
    }

    public TemplateDTOBuilder setAom(byte[] aom) {
        this.aom = aom;
        return this;
    }

    public TemplateDTOBuilder setAobcVO(byte[] aobcVO) {
        this.aobcVO = aobcVO;
        return this;
    }

    public TemplateDTO createTemplateDTO() {
        return new TemplateDTO(templateId, arcehtypeId, name, description, entryType, archetype, aom, aobcVO);
    }
}