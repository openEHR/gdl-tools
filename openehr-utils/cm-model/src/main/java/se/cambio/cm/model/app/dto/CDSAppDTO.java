package se.cambio.cm.model.app.dto;

import se.cambio.cm.model.util.CMElement;

import java.util.Date;

public class CDSAppDTO implements CMElement {
    private static final long serialVersionUID = 2012054542L;
    private String id;
    private String format;
    private String source;
    private Date lastUpdate;

    public CDSAppDTO() {}

    @Override
    public String getId() {
        return id;
    }

    @Override
    public String getFormat() {
        return format;
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
    public String getSource() {
        return source;
    }

    @Override
    public void setSource(String source) {
        this.source = source;
    }

    @Override
    public Date getLastUpdate() {
        return lastUpdate;
    }

    @Override
    public void setLastUpdate(Date lastUpdate) {
        this.lastUpdate = lastUpdate;
    }
}