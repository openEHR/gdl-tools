package se.cambio.cm.model.study.dto;

import se.cambio.cm.model.util.CMElement;

import java.util.Date;

public class StudyDTO implements CMElement{
    private String id;
    private String format;
    private String source;
    private Date lastUpdate;

    public StudyDTO() { }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public String getFormat() {
        return format;
    }

    @Override
    public String getSource() {
        return source;
    }

    @Override
    public Date getLastUpdate() {
        return lastUpdate;
    }

    @Override
    public void setId(String id) {
        this.id = id;
    }

    @Override
    public void setFormat(String format) {
        this.format = format;
    }

    @Override
    public void setSource(String source) {
        this.source = source;
    }

    @Override
    public void setLastUpdate(Date lastUpdate) {
        this.lastUpdate = lastUpdate;
    }
}
