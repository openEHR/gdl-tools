package se.cambio.cds.gdl.model.readable.rule.lines;

import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeElementRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.EqualityComparisonOperatorRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.NullValueRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.StaticTextRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ConditionRuleLine;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRLanguageManager;


public class ElementComparisonWithNullValueConditionRuleLine extends ExpressionRuleLine implements ConditionRuleLine{

    private ArchetypeElementRuleLineElement archetypeElementRuleLineElement = null;
    private EqualityComparisonOperatorRuleLineElement equalityComparisonOperatorRuleLineElement = null;
    private NullValueRuleLineElement nullValueRuleLineElement = null;


    public ElementComparisonWithNullValueConditionRuleLine() {
	super(OpenEHRLanguageManager.getMessage("CompareElementWithNullValue"), 
		OpenEHRLanguageManager.getMessage("CompareElementWithNullValueDesc"));
	archetypeElementRuleLineElement = new ArchetypeElementRuleLineElement(this);
	equalityComparisonOperatorRuleLineElement = new EqualityComparisonOperatorRuleLineElement(this);
	nullValueRuleLineElement = new NullValueRuleLineElement(this);

	getRuleLineElements().add(new StaticTextRuleLineElement(OpenEHRLanguageManager.getMessage("ElementRLE")));
	getRuleLineElements().add(archetypeElementRuleLineElement);
	getRuleLineElements().add(new StaticTextRuleLineElement(OpenEHRLanguageManager.getMessage("NullValueRLE")));
	getRuleLineElements().add(equalityComparisonOperatorRuleLineElement);
	getRuleLineElements().add(nullValueRuleLineElement);
    }

    public ArchetypeElementRuleLineElement getArchetypeElementRuleLineElement(){
	return archetypeElementRuleLineElement;
    }

    public EqualityComparisonOperatorRuleLineElement getEqualityComparisonOperatorRuleLineElement(){
	return equalityComparisonOperatorRuleLineElement;
    }

    public NullValueRuleLineElement getNullValueRuleLineElement(){
	return nullValueRuleLineElement;
    }

    public ExpressionItem toExpressionItem() throws IllegalStateException{
	ArchetypeElementVO archetypeElementVO = getArchetypeElementRuleLineElement().getArchetypeElementVO();
	if (archetypeElementVO!=null){
	    String gtCode = 
		    getArchetypeElementRuleLineElement().getValue().getValue();
	    DataValue nullValue = 
		    getNullValueRuleLineElement().getValue();
	    if (nullValue==null){
		throw new IllegalStateException("No null value set");
	    }
	    String dataValueStr = nullValue.toString();

	    OperatorKind operatorKind =
		    getEqualityComparisonOperatorRuleLineElement().getValue();
	    if (operatorKind==null){
		throw new IllegalStateException("No operator kind set");
	    }
	    return new BinaryExpression(
		    new Variable(
			    gtCode, 
			    archetypeElementVO.getName(), 
			    archetypeElementVO.getPath(), 
			    OpenEHRConst.NULL_FLAVOR_ATTRIBUTE),
			    new ConstantExpression(dataValueStr),
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