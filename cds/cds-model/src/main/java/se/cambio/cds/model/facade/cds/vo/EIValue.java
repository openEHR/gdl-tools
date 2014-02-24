package se.cambio.cds.model.facade.cds.vo;

import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.model.facade.execution.vo.RuleReference;

import java.util.HashSet;
import java.util.Set;

/**
 * User: Iago.Corbal
 * Date: 2013-11-26
 * Time: 15:01
 */
public class EIValue{
    private String dv;
    private Set<RuleReference> ruleReferences = new HashSet<RuleReference>();
    private OperatorKind operatorKind;

    public EIValue(String dv, OperatorKind operatorKind) {
        this.dv = dv;
        this.operatorKind = operatorKind;
    }

    public String getDv() {
        return dv;
    }

    public void setDv(String dv) {
        this.dv = dv;
    }

    public Set<RuleReference> getRuleReferences() {
        return ruleReferences;
    }

    public void setRuleReferences(Set<RuleReference> ruleReferences) {
        this.ruleReferences = ruleReferences;
    }

    public OperatorKind getOperatorKind() {
        return operatorKind;
    }

    public void setOperatorKind(OperatorKind operatorKind) {
        this.operatorKind = operatorKind;
    }
}