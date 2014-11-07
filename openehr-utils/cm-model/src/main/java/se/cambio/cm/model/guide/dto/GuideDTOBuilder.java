package se.cambio.cm.model.guide.dto;

import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Date;

public class GuideDTOBuilder {
    private String id;
    private String format;
    private String source;
    private byte[] guideObject;
    private byte[] compiledGuide;
    private Date lastUpdate;

    public GuideDTOBuilder setId(String id) {
        this.id = id;
        return this;
    }

    public GuideDTOBuilder setFormat(String format) {
        this.format = format;
        return this;
    }

    public GuideDTOBuilder setSource(String source) {
        this.source = source;
        return this;
    }

    public GuideDTOBuilder setGuideObject(byte[] guideObject) {
        this.guideObject = guideObject;
        return this;
    }

    public GuideDTOBuilder setCompiledGuide(byte[] compiledGuide) {
        this.compiledGuide = compiledGuide;
        return this;
    }

    public GuideDTOBuilder setLastUpdate(Date lastUpdate) {
        this.lastUpdate = lastUpdate;
        return this;
    }

    public GuideDTO createGuideDTO() throws InternalErrorException {
        checkMissingAttributes();
        return new GuideDTO(id, format, source, guideObject, compiledGuide, lastUpdate);
    }

    private void checkMissingAttributes() throws InternalErrorException {
        if (id == null){
            throw new InternalErrorException(new Exception("No id specified for cmElement"));
        }
        if (format == null){
            throw new InternalErrorException(new Exception("No format specified for cmElement"));
        }
        if (source == null){
            throw new InternalErrorException(new Exception("No source specified for cmElement"));
        }
        if (lastUpdate == null){
            throw new InternalErrorException(new Exception("No lastUpdate specified for cmElement"));
        }
    }
}