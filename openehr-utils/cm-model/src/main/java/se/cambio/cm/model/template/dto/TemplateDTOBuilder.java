package se.cambio.cm.model.template.dto;

import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Date;

public class TemplateDTOBuilder {
    private String id;
    private String format;
    private String arcehtypeId;
    private String source;
    private byte[] aom;
    private byte[] aobcVO;
    private Date lastUpdate;

    public TemplateDTOBuilder setId(String id) {
        this.id = id;
        return this;
    }

    public TemplateDTOBuilder setFormat(String format) {
        this.format = format;
        return this;
    }

    public TemplateDTOBuilder setArchetypeId(String archetypeId) {
        this.arcehtypeId = archetypeId;
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

    public TemplateDTO createTemplateDTO() throws InternalErrorException {
        checkMissingAttributes();
        return new TemplateDTO(id, format, arcehtypeId, source, aom, aobcVO, lastUpdate);
    }

    private void checkMissingAttributes() throws InternalErrorException {
        if (id == null) {
            throw new InternalErrorException(new Exception("No id specified for cmElement"));
        }
        if (format == null) {
            throw new InternalErrorException(new Exception("No format specified for cmElement"));
        }
        if (source == null) {
            throw new InternalErrorException(new Exception("No source specified for cmElement"));
        }
        if (lastUpdate == null) {
            throw new InternalErrorException(new Exception("No lastUpdate specified for cmElement"));
        }
    }
}