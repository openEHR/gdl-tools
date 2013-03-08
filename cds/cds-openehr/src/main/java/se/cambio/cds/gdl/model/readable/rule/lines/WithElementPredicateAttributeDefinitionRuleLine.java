package se.cambio.cds.gdl.model.readable.rule.lines;

import org.openehr.rm.datatypes.basic.DataValue;

import se.cambio.cds.gdl.model.expression.BinaryExpression;
import se.cambio.cds.gdl.model.expression.ConstantExpression;
import se.cambio.cds.gdl.model.expression.ExpressionItem;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.gdl.model.expression.Variable;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeElementRuleLineDefinitionElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.DataValueRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.PredicateComparisonOperatorRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.StaticTextRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ArchetypeReferenceRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.DefinitionsRuleLine;
import se.cambio.cds.model.facade.execution.vo.ArchetypeReference;
import se.cambio.cds.openehr.model.archetypeelement.vo.ArchetypeElementVO;
import se.cambio.cds.openehr.util.OpenEHRLanguageManager;
import se.cambio.cds.util.DVUtil;


public class WithElementPredicateAttributeDefinitionRuleLine extends ExpressionRuleLine implements ArchetypeReferenceRuleLine, DefinitionsRuleLine{

    private ArchetypeElementRuleLineDefinitionElement archetypeElementRuleLineDefinitionElement = null;
    private PredicateComparisonOperatorRuleLineElement comparisonOperatorRuleLineElement = null;
    private DataValueRuleLineElement dataValueRuleLineElement = null;
    

    public WithElementPredicateAttributeDefinitionRuleLine() {
	super(OpenEHRLanguageManager.getMessage("ElementPredicateAttribute"),
		OpenEHRLanguageManager.getMessage("ElementPredicateAttributeDesc"));
	archetypeElementRuleLineDefinitionElement = new ArchetypeElementRuleLineDefinitionElement(this);
	comparisonOperatorRuleLineElement = new PredicateComparisonOperatorRuleLineElement(this);
	dataValueRuleLineElement = new DataValueRuleLineElement(this);
	
	getRuleLineElements().add(new StaticTextRuleLineElement(OpenEHRLanguageManager.getMessage("WithElementRLE")));
	getRuleLineElements().add(archetypeElementRuleLineDefinitionElement);
	getRuleLineElements().add(comparisonOperatorRuleLineElement);
	getRuleLineElements().add(dataValueRuleLineElement);
    }

    public ArchetypeElementRuleLineDefinitionElement getArchetypeElementRuleLineDefinitionElement() {
        return archetypeElementRuleLineDefinitionElement;
    }

    public PredicateComparisonOperatorRuleLineElement getComparisonOperatorRuleLineElement() {
        return comparisonOperatorRuleLineElement;
    }
    
    public DataValueRuleLineElement getDataValueRuleLineElement() {
        return dataValueRuleLineElement;
    }

    @Override
    public ArchetypeReference getArchetypeReference() {
	return getArchetypeInstantiationRuleLine().
		getArchetypeReferenceRuleLineDefinitionElement().getValue();
    }
    
    public ArchetypeInstantiationRuleLine getArchetypeInstantiationRuleLine() {
	return (ArchetypeInstantiationRuleLine)getParentRuleLine();
    }

    @Override
    public ExpressionItem toExpressionItem() {
	ArchetypeElementVO archetypeElementVO = getArchetypeElementRuleLineDefinitionElement().getValue();
	String path = archetypeElementVO.getPath();
	DataValue dataValue = 
		getDataValueRuleLineElement().getValue();
	OperatorKind operatorKind =
		getComparisonOperatorRuleLineElement().getValue();
	ConstantExpression constantExpression = null;
	if (dataValue!=null){
	    constantExpression = DVUtil.convertToExpression(dataValue);
	}else{
	    throw new IllegalStateException("No data value set");
	}
	return new BinaryExpression(
			new Variable(null,archetypeElementVO.getName(), path),
			constantExpression,
			operatorKind);
    }
    
}/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  1.1 (the 'License'); you may not use this file except in compliance with
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