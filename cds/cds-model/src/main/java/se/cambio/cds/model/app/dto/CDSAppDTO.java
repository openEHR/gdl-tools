package se.cambio.cds.model.app.dto;

import java.io.Serializable;
import java.util.Date;

public class CDSAppDTO implements Serializable {
    private static final long serialVersionUID = 2012054542L;

    private String cdaAppId;
    private String appSrc;
    private Date lastUpdate;

    public CDSAppDTO(String cdaAppId, String appSrc, Date lastUpdate) {
        this.cdaAppId = cdaAppId;
        this.appSrc = appSrc;
        this.lastUpdate = lastUpdate;
    }

    public String getCdaAppId() {
        return cdaAppId;
    }

    public String getAppSrc() {
        return appSrc;
    }

    public Date getLastUpdate() {
        return lastUpdate;
    }
}