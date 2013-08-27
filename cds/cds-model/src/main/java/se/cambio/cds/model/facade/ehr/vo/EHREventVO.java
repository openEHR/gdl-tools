package se.cambio.cds.model.facade.ehr.vo;

/**
 * User: Iago.Corbal
 * Date: 2013-08-15
 * Time: 19:21
 */
public class EHREventVO {

    private String ehrId = null;

    public EHREventVO(String ehrId) {
        this.ehrId = ehrId;
    }

    public String getEhrId() {
        return ehrId;
    }

    public void setEhrId(String ehrId) {
        this.ehrId = ehrId;
    }
}
