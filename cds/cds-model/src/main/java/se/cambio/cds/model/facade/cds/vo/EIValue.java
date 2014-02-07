package se.cambio.cds.model.facade.cds.vo;

import se.cambio.cds.gdl.model.expression.OperatorKind;

/**
 * User: Iago.Corbal
 * Date: 2013-11-26
 * Time: 15:01
 */
public class EIValue{
    private String dv;
    private String guideId;
    private String gtCode;
    private OperatorKind operatorKind;

    public EIValue(String dv, String gtCode, String guideId, OperatorKind operatorKind) {
        this.dv = dv;
        this.gtCode = gtCode;
        this.guideId = guideId;
        this.operatorKind = operatorKind;
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

    public OperatorKind getOperatorKind() {
        return operatorKind;
    }

    public void setOperatorKind(OperatorKind operatorKind) {
        this.operatorKind = operatorKind;
    }
}