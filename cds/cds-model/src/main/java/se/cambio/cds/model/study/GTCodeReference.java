package se.cambio.cds.model.study;

import java.io.Serializable;

/**
 * User: iago.corbal
 * Date: 2014-09-04
 * Time: 12:03
 */
public class GTCodeReference implements Serializable{

    private String guideId;
    private String gtCode;

    public GTCodeReference(String guideId, String gtCode) {
        this.guideId = guideId;
        this.gtCode = gtCode;
    }

    public String getGuideId() {
        return guideId;
    }

    public void setGuideId(String guideId) {
        this.guideId = guideId;
    }

    public String getGtCode() {
        return gtCode;
    }

    public void setGtCode(String gtCode) {
        this.gtCode = gtCode;
    }
}
