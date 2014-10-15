package se.cambio.cds.model.orderset.dto;

import se.cambio.openehr.model.util.CMElement;

import java.util.Date;

public class OrderSetDTO implements CMElement{
    private String id;
    private String source;
    private Date lastUpdate;

    public OrderSetDTO(String id) {
        this.id = id;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public Date getLastUpdate() {
        return lastUpdate;
    }

    public void setLastUpdate(Date lastUpdate) {
        this.lastUpdate = lastUpdate;
    }
}
