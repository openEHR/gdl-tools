package se.cambio.cds.gdl.model.readable.rule.lines;

import se.cambio.cds.gdl.model.expression.BinaryExpression;
import se.cambio.cds.gdl.model.expression.ExpressionItem;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.gdl.model.expression.Variable;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeElementAttributeRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.AttributeComparisonOperatorRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ExpressionRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.StaticTextRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ConditionRuleLine;
import se.cambio.openehr.util.OpenEHRLanguageManager;


public class ElementAttributeComparisonConditionRuleLine extends ExpressionRuleLine implements ConditionRuleLine{

    private ArchetypeElementAttributeRuleLineElement archetypeElementAttributeRuleLineElement = null;
    private AttributeComparisonOperatorRuleLineElement comparisonOperatorRuleLineElement = null;
    private ExpressionRuleLineElement expressionRuleLineElement = null;


    public ElementAttributeComparisonConditionRuleLine() {
	super(OpenEHRLanguageManager.getMessage("CompareElementAttributeWithExpression"), 
		OpenEHRLanguageManager.getMessage("CompareElementAttributeWithExpressionDesc"));
	archetypeElementAttributeRuleLineElement = new ArchetypeElementAttributeRuleLineElement(this);
	comparisonOperatorRuleLineElement = new AttributeComparisonOperatorRuleLineElement(this);
	expressionRuleLineElement = new ExpressionRuleLineElement(this);

	getRuleLineElements().add(new StaticTextRuleLineElement(OpenEHRLanguageManager.getMessage("ElementRLE")));
	getRuleLineElements().add(archetypeElementAttributeRuleLineElement);
	getRuleLineElements().add(comparisonOperatorRuleLineElement);
	getRuleLineElements().add(expressionRuleLineElement);
    }

    public ArchetypeElementAttributeRuleLineElement getArchetypeElementAttributeRuleLineElement(){
	return archetypeElementAttributeRuleLineElement;
    }

    public AttributeComparisonOperatorRuleLineElement getComparisonOperatorRuleLineElement(){
	return comparisonOperatorRuleLineElement;
    }

    public ExpressionRuleLineElement getExpressionRuleLineElement(){
	return expressionRuleLineElement;
    }

    public ExpressionItem toExpressionItem() throws IllegalStateException{
	if (archetypeElementAttributeRuleLineElement!=null &&
		getArchetypeElementAttributeRuleLineElement().getValue()!=null){
	    String gtCode = 
		    getArchetypeElementAttributeRuleLineElement().getValue().getValue().getValue();
	    ExpressionRuleLineElement expressionRuleLineElement = 
		    getExpressionRuleLineElement();
	    OperatorKind operatorKind =
		    getComparisonOperatorRuleLineElement().getValue();
	    if (operatorKind==null){
		throw new IllegalStateException("No operator kind set");
	    }
	    Variable var = 
		    new Variable(gtCode, null, null, getArchetypeElementAttributeRuleLineElement().getAttribute());
	    if (expressionRuleLineElement.getValue()==null){
		throw new IllegalStateException("No expression set");
	    }
	    return new BinaryExpression(
		    var,
		    expressionRuleLineElement.getValue(),
		    operatorKind);

	}else{
	    throw new IllegalStateException("Element instance not found for"+ this.toString());
	}
    }
}/*
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