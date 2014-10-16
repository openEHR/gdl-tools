package se.cambio.openehr.model.template.dto;

import java.util.Date;

public class TemplateDTOBuilder {
    private String id;
    private String arcehtypeId;
    private String name;
    private String description;
    private String rmName;
    private String source;
    private byte[] aom;
    private byte[] aobcVO;
    private Date lastUpdate;

    public TemplateDTOBuilder setId(String id) {
        this.id = id;
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

    public TemplateDTOBuilder setRmName(String rmName) {
        this.rmName = rmName;
        return this;
    }

    public TemplateDTOBuilder setSource(String source) {
        this.source = source;
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

    public TemplateDTOBuilder setLastUpdate(Date lastUpdate) {
        this.lastUpdate = lastUpdate;
        return this;
    }

    public TemplateDTO createTemplateDTO() {
        return new TemplateDTO(id, arcehtypeId, name, description, rmName, source, aom, aobcVO, lastUpdate);
    }
}