package se.cambio.cds.gdl.model.readable.rule.lines;

import se.cambio.cds.gdl.model.expression.AssignmentExpression;
import se.cambio.cds.gdl.model.expression.Variable;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeElementAttributeRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ExpressionRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.StaticTextRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ActionRuleLine;
import se.cambio.openehr.util.OpenEHRLanguageManager;


public class SetElementAttributeActionRuleLine extends AssignmentExpressionRuleLine implements ActionRuleLine {

    private ArchetypeElementAttributeRuleLineElement archetypeElementAttributeRuleLineElement = null;
    private ExpressionRuleLineElement expressionRuleLineElement = null;


    public SetElementAttributeActionRuleLine() {
        super(OpenEHRLanguageManager.getMessage("SetElementAttribute"),
                OpenEHRLanguageManager.getMessage("SetElementAttributeDesc"));
        archetypeElementAttributeRuleLineElement = new ArchetypeElementAttributeRuleLineElement(this);
        expressionRuleLineElement = new ExpressionRuleLineElement(this);
        getRuleLineElements().add(new StaticTextRuleLineElement(this, "SetElementRLE"));
        getRuleLineElements().add(archetypeElementAttributeRuleLineElement);
        getRuleLineElements().add(new StaticTextRuleLineElement(this, "ToRLE"));
        getRuleLineElements().add(expressionRuleLineElement);
    }


    public ArchetypeElementAttributeRuleLineElement getArchetypeElementAttributeRuleLineElement() {
        return archetypeElementAttributeRuleLineElement;
    }


    public ExpressionRuleLineElement getExpressionRuleLineElement() {
        return expressionRuleLineElement;
    }


    public AssignmentExpression toAssignmentExpression() throws IllegalStateException {
        if (archetypeElementAttributeRuleLineElement.getValue() == null || archetypeElementAttributeRuleLineElement.getValue().getValue() == null) {
            throw new IllegalStateException("No variable set");
        }
        Variable var = new Variable(
                archetypeElementAttributeRuleLineElement.getValue().getValue().getValue(),
                null,
                null,
                archetypeElementAttributeRuleLineElement.getAttribute());
        return new AssignmentExpression(
                var,
                expressionRuleLineElement.getValue());
    }

}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 2.0/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  2.0 (the 'License'); you may not use this file except in compliance with
 *  the License. You may obtain a copy of the License at
 *  http://www.mozilla.org/MPL/
 *
 *  Software distributed under the License is distributed on an 'AS IS' basis,
 *  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 *  for the specific language governing rights and limitations under the
 *  License.
 *
 *
 *  The Initial Developers of the Original Code are Iago Corbal and Rong Chen.
 *  Portions created by the Initial Developer are Copyright (C) 2012-2013
 *  the Initial Developer. All Rights Reserved.
 *
 *  Contributor(s):
 *
 * Software distributed under the License is distributed on an 'AS IS' basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 *  ***** END LICENSE BLOCK *****
 */