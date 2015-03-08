package se.cambio.cds.gdl.model.readable.rule.lines.elements;

import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.util.misc.CDSLanguageManager;

public class FiredRuleReferenceRuleElement extends RuleLineElementWithValue<GTCodeRuleLineElement> {
    public FiredRuleReferenceRuleElement(RuleLine ruleLine) {
        super(ruleLine, CDSLanguageManager.getMessage("RuleFired"));
    }

    public String toString(){
        if (getValue() != null) {
            return getValue().toString();
        } else {
            return getText();
        }
    }

    @Override
    public String toHTMLString(String lang) {
        return "<font color='#4f81bd'><b>"+toString()+"</b></font>";
    }
}
