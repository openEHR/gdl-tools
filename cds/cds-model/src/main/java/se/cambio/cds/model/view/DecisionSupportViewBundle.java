package se.cambio.cds.model.view;

/**
 * User: iago.corbal
 * Date: 2014-09-23
 * Time: 14:55
 */
public class DecisionSupportViewBundle {
    private DecisionSupportView decisionSupportView = null;
    private String dsvSrc = null;

    public DecisionSupportViewBundle(DecisionSupportView decisionSupportView, String dsvSrc) {
        this.decisionSupportView = decisionSupportView;
        this.dsvSrc = dsvSrc;
    }

    public DecisionSupportView getDecisionSupportView() {
        return decisionSupportView;
    }

    public void setDecisionSupportView(DecisionSupportView decisionSupportView) {
        this.decisionSupportView = decisionSupportView;
    }

    public String getDsvSrc() {
        return dsvSrc;
    }

    public void setDsvSrc(String dsvSrc) {
        this.dsvSrc = dsvSrc;
    }
}