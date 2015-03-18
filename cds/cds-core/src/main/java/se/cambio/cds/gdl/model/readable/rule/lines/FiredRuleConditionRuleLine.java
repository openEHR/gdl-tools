package se.cambio.cds.gdl.model.readable.rule.lines;

import se.cambio.cds.gdl.model.expression.ExpressionItem;
import se.cambio.cds.gdl.model.expression.UnaryExpression;
import se.cambio.cds.gdl.model.expression.Variable;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.FiredRuleOperatorRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.FiredRuleReferenceRuleElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.StaticTextRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ConditionRuleLine;
import se.cambio.cds.util.misc.CDSLanguageManager;

public class FiredRuleConditionRuleLine extends ExpressionRuleLine implements ConditionRuleLine {

    private final FiredRuleReferenceRuleElement firedRuleReferenceRuleElement;
    private final FiredRuleOperatorRuleLineElement firedRuleOperatorRuleLineElement;

    public FiredRuleConditionRuleLine() {
        super(CDSLanguageManager.getMessage("FiredRuleCondition"),
                CDSLanguageManager.getMessage("FiredRuleConditionDesc"));
        firedRuleReferenceRuleElement = new FiredRuleReferenceRuleElement(this);
        firedRuleOperatorRuleLineElement = new FiredRuleOperatorRuleLineElement(this);
        getRuleLineElements().add(new StaticTextRuleLineElement(CDSLanguageManager.getMessage("Rule")));
        getRuleLineElements().add(firedRuleReferenceRuleElement);
        getRuleLineElements().add(firedRuleOperatorRuleLineElement);
    }

    public FiredRuleReferenceRuleElement getFiredRuleReferenceRuleElement() {
        return firedRuleReferenceRuleElement;
    }

    public FiredRuleOperatorRuleLineElement getFiredRuleOperatorRuleLineElement() {
        return firedRuleOperatorRuleLineElement;
    }

    @Override
    public ExpressionItem toExpressionItem() throws IllegalStateException {
        String gtCode = firedRuleReferenceRuleElement.getValue().getValue();
        return new UnaryExpression(new Variable(gtCode), firedRuleOperatorRuleLineElement.getValue());
    }
}
