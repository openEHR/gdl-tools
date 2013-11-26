package se.cambio.cds.model.facade.cds.vo;

/**
 * User: Iago.Corbal
 * Date: 2013-11-26
 * Time: 15:01
 */
public class EIValue{
    private String dv;
    private String guideId;
    private String gtCode;

    public EIValue(String dv, String gtCode, String guideId) {
        this.dv = dv;
        this.gtCode = gtCode;
        this.guideId = guideId;
    }

    public String getDv() {
        return dv;
    }

    public String getGtCode() {
        return gtCode;
    }

    public String getGuideId() {
        return guideId;
    }

    public void setDv(String dv) {
        this.dv = dv;
    }

    public void setGtCode(String gtCode) {
        this.gtCode = gtCode;
    }

    public void setGuideId(String guideId) {
        this.guideId = guideId;
    }
}