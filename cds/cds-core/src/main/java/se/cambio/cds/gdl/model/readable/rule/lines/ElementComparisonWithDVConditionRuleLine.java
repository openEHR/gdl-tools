package se.cambio.cds.gdl.model.readable.rule.lines;

import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeElementRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.DataValueRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ElementComparisonOperatorRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.StaticTextRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ArchetypeElementRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ConditionRuleLine;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.util.DVUtil;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRLanguageManager;


public class ElementComparisonWithDVConditionRuleLine extends ExpressionRuleLine implements ArchetypeElementRuleLine, ConditionRuleLine{

    private ArchetypeElementRuleLineElement archetypeElementRuleLineElement = null;
    private ElementComparisonOperatorRuleLineElement comparisonOperatorRuleLineElement = null;
    private DataValueRuleLineElement dataValueRuleLineElement = null;


    public ElementComparisonWithDVConditionRuleLine() {
	super(OpenEHRLanguageManager.getMessage("CompareElementWithDataValue"), 
		OpenEHRLanguageManager.getMessage("CompareElementWithDataValueDesc"));
	archetypeElementRuleLineElement = new ArchetypeElementRuleLineElement(this);
	comparisonOperatorRuleLineElement = new ElementComparisonOperatorRuleLineElement(this);
	dataValueRuleLineElement = new DataValueRuleLineElement(this);

	getRuleLineElements().add(new StaticTextRuleLineElement(OpenEHRLanguageManager.getMessage("ElementRLE")));
	getRuleLineElements().add(archetypeElementRuleLineElement);
	getRuleLineElements().add(comparisonOperatorRuleLineElement);
	getRuleLineElements().add(dataValueRuleLineElement);
    }

    public ArchetypeElementRuleLineElement getArchetypeElementRuleLineElement(){
	return archetypeElementRuleLineElement;
    }

    @Override
    public ArchetypeReference getArchetypeReference() {
	return archetypeElementRuleLineElement.getArchetypeReference();
    }

    @Override
    public ArchetypeElementVO getArchetypeElement() {
	return archetypeElementRuleLineElement.getArchetypeElementVO();
    }
    
    public ElementComparisonOperatorRuleLineElement getComparisonOperatorRuleLineElement(){
	return comparisonOperatorRuleLineElement;
    }

    public DataValueRuleLineElement getDataValueRuleLineElement(){
	return dataValueRuleLineElement;
    }

    public ExpressionItem toExpressionItem() throws IllegalStateException{
	ArchetypeElementVO archetypeElementVO = getArchetypeElementRuleLineElement().getArchetypeElementVO();
	if (archetypeElementVO!=null){
	    String gtCode = 
		    getArchetypeElementRuleLineElement().getValue().getValue();
	    DataValue dataValue = 
		    getDataValueRuleLineElement().getValue();
	    ConstantExpression constantExpression = null;
	    if (dataValue!=null){
		constantExpression = DVUtil.convertToExpression(dataValue);
	    }else{
		throw new IllegalStateException("No data value set");
	    }
	    OperatorKind operatorKind =
		    getComparisonOperatorRuleLineElement().getValue();
	    if (operatorKind==null){
		throw new IllegalStateException("No operator set");
	    }
	    return new BinaryExpression(
		    new Variable(gtCode,null, archetypeElementVO.getName()),
		    constantExpression,
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