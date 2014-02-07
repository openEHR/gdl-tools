package se.cambio.cds.gdl.model.readable.rule.lines;

import se.cambio.cds.gdl.model.expression.BinaryExpression;
import se.cambio.cds.gdl.model.expression.ExpressionItem;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.gdl.model.expression.Variable;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeElementRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ElementComparisonOperatorRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.StaticTextRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ConditionRuleLine;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRLanguageManager;


public class ElementComparisonWithElementConditionRuleLine extends ExpressionRuleLine implements ConditionRuleLine{

    private ArchetypeElementRuleLineElement archetypeElementRuleLineElement = null;
    private ElementComparisonOperatorRuleLineElement comparisonOperatorRuleLineElement = null;
    private ArchetypeElementRuleLineElement archetypeElementRuleLineElement2 = null;


    public ElementComparisonWithElementConditionRuleLine() {
	super(OpenEHRLanguageManager.getMessage("CompareElementWithElement"), 
		OpenEHRLanguageManager.getMessage("CompareElementWithElementDesc"));
	archetypeElementRuleLineElement = new ArchetypeElementRuleLineElement(this);
	comparisonOperatorRuleLineElement = new ElementComparisonOperatorRuleLineElement(this);
	archetypeElementRuleLineElement2 = new ArchetypeElementRuleLineElement(this);

	getRuleLineElements().add(new StaticTextRuleLineElement(OpenEHRLanguageManager.getMessage("ElementRLE")));
	getRuleLineElements().add(archetypeElementRuleLineElement);
	getRuleLineElements().add(comparisonOperatorRuleLineElement);
	getRuleLineElements().add(archetypeElementRuleLineElement2);
    }

    public ArchetypeElementRuleLineElement getArchetypeElementRuleLineElement(){
	return archetypeElementRuleLineElement;
    }

    public ElementComparisonOperatorRuleLineElement getComparisonOperatorRuleLineElement(){
	return comparisonOperatorRuleLineElement;
    }

    public ArchetypeElementRuleLineElement getSecondArchetypeElementRuleLineElement(){
	return archetypeElementRuleLineElement2;
    }

    public ExpressionItem toExpressionItem() throws IllegalStateException{
	ArchetypeElementVO archetypeElementVO = getArchetypeElementRuleLineElement().getArchetypeElementVO();
	if (archetypeElementVO!=null){
	    String gtCode = 
		    getArchetypeElementRuleLineElement().getValue().getValue();
	    if (getSecondArchetypeElementRuleLineElement().getValue()==null){
		throw new IllegalStateException("No expression set");
	    }
	    String secondGtCode = 
		    getSecondArchetypeElementRuleLineElement().getValue().getValue();
	    OperatorKind operatorKind =
		    getComparisonOperatorRuleLineElement().getValue();
	    if (operatorKind==null){
		throw new IllegalStateException("No operator kind set");
	    }
	    return new BinaryExpression(
		    new Variable(gtCode, null, archetypeElementVO.getName()),
		    new Variable(secondGtCode),
		    operatorKind);
	}else{
	    throw new IllegalStateException("Invalid rule line: "+getArchetypeElementRuleLineElement());
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