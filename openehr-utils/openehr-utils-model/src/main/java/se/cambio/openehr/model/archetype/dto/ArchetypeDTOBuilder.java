package se.cambio.openehr.model.archetype.dto;

import java.util.Date;

public class ArchetypeDTOBuilder {
    private String id;
    private String name;
    private String description;
    private String rmName;
    private String source;
    private byte[] aom;
    private byte[] aobcVO;
    private Date lastUpdate;

    public ArchetypeDTOBuilder setId(String id) {
        this.id = id;
        return this;
    }

    public ArchetypeDTOBuilder setName(String name) {
        this.name = name;
        return this;
    }

    public ArchetypeDTOBuilder setDescription(String description) {
        this.description = description;
        return this;
    }

    public ArchetypeDTOBuilder setRmName(String rmName) {
        this.rmName = rmName;
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

    public ArchetypeDTO createArchetypeDTO() {
        return new ArchetypeDTO(id, name, description, rmName, source, aom, aobcVO, lastUpdate);
    }
}