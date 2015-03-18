package se.cambio.cds.model.instance;

import se.cambio.cds.model.facade.execution.vo.RuleReference;

public class FiredRuleReference extends RuleReference{

    public FiredRuleReference(String guideId, String gtCode) {
        super(guideId, gtCode);
    }

    public FiredRuleReference(String ruleId) {
        super(ruleId);
    }

    @Override
    public String getGuideId() {
        return super.getGuideId();
    }

    @Override
    public void setGuideId(String guideId) {
        super.setGuideId(guideId);
    }

    public String getGtCode() {
        return super.getGTCode();
    }

    public void setGtCode(String gtCode) {
        super.setGTCode(gtCode);
    }
}
