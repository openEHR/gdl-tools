package se.cambio.cds.model.orderset.dto;

import java.util.Date;

public class OrderSetDTO {
    private String orderSetId;
    private String orderSetSrc;
    private Date lastUpdate;

    public OrderSetDTO(String orderSetId) {
        this.orderSetId = orderSetId;
    }

    public String getOrderSetId() {
        return orderSetId;
    }

    public void setOrderSetId(String orderSetId) {
        this.orderSetId = orderSetId;
    }

    public String getOrderSetSrc() {
        return orderSetSrc;
    }

    public void setOrderSetSrc(String orderSetSrc) {
        this.orderSetSrc = orderSetSrc;
    }

    public Date getLastUpdate() {
        return lastUpdate;
    }

    public void setLastUpdate(Date lastUpdate) {
        this.lastUpdate = lastUpdate;
    }
}
