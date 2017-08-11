package se.cambio.cm.model.archetype.dto;

import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Date;

public class ArchetypeDTOBuilder {
    private String id;
    private String format;
    private String source;
    private byte[] aom;
    private byte[] aobcVO;
    private Date lastUpdate;

    public ArchetypeDTOBuilder setId(String id) {
        this.id = id;
        return this;
    }

    public ArchetypeDTOBuilder setFormat(String format) {
        this.format = format;
        return this;
    }


    public ArchetypeDTOBuilder setSource(String source) {
        this.source = source;
        return this;
    }

    public ArchetypeDTOBuilder setAom(byte[] aom) {
        this.aom = aom;
        return this;
    }

    public ArchetypeDTOBuilder setAobcVO(byte[] aobcVO) {
        this.aobcVO = aobcVO;
        return this;
    }

    public ArchetypeDTOBuilder setLastUpdate(Date lastUpdate) {
        this.lastUpdate = lastUpdate;
        return this;
    }

    public ArchetypeDTO createArchetypeDTO() throws InternalErrorException {
        checkMissingAttributes();
        return new ArchetypeDTO(id, format, source, aom, aobcVO, lastUpdate);
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