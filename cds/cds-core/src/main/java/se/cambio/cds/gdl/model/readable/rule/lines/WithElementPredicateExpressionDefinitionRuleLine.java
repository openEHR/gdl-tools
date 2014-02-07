package se.cambio.cds.gdl.model.readable.rule.lines;

import se.cambio.cds.gdl.model.expression.BinaryExpression;
import se.cambio.cds.gdl.model.expression.ExpressionItem;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.gdl.model.expression.Variable;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.*;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ArchetypeReferenceRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.DefinitionsRuleLine;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRLanguageManager;


public class WithElementPredicateExpressionDefinitionRuleLine extends ExpressionRuleLine implements ArchetypeReferenceRuleLine, DefinitionsRuleLine{

    private PredicateArchetypeElementAttributeRuleLineElement archetypeElementAttributeRuleLineDefinitionElement = null;
    private PredicateAttributeComparisonOperatorRuleLineElement comparisonOperatorRuleLineElement = null;
    private ExpressionRuleLineElement expressionRuleLineElement = null;

    private ArchetypeElementRuleLineDefinitionElement _archetypeElementRuleLineDefinitionElement = null;

    public WithElementPredicateExpressionDefinitionRuleLine(
            ArchetypeInstantiationRuleLine archetypeInstantiationRuleLine) {
        super(OpenEHRLanguageManager.getMessage("ElementPredicateExpression"),
                OpenEHRLanguageManager.getMessage("ElementPredicateExpressionDesc"));
        archetypeElementAttributeRuleLineDefinitionElement = new PredicateArchetypeElementAttributeRuleLineElement(this);
        comparisonOperatorRuleLineElement = new PredicateAttributeComparisonOperatorRuleLineElement(this);
        expressionRuleLineElement = new ExpressionRuleLineElement(this);

        if (archetypeInstantiationRuleLine!=null){
            archetypeInstantiationRuleLine.addChildRuleLine(this);
        }

        getRuleLineElements().add(new StaticTextRuleLineElement(OpenEHRLanguageManager.getMessage("WithElementRLE")));
        getRuleLineElements().add(archetypeElementAttributeRuleLineDefinitionElement);
        getRuleLineElements().add(comparisonOperatorRuleLineElement);
        getRuleLineElements().add(expressionRuleLineElement);
    }

    public PredicateArchetypeElementAttributeRuleLineElement getArchetypeElementAttributeRuleLineDefinitionElement() {
        return archetypeElementAttributeRuleLineDefinitionElement;
    }

    public PredicateAttributeComparisonOperatorRuleLineElement getComparisonOperatorRuleLineElement() {
        return comparisonOperatorRuleLineElement;
    }

    public ExpressionRuleLineElement getExpressionRuleLineElement(){
        return expressionRuleLineElement;
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
        ArchetypeElementVO archetypeElementVO = getArchetypeElementAttributeRuleLineDefinitionElement().getValue();
        String attribute = getArchetypeElementAttributeRuleLineDefinitionElement().getAttribute();
        String path =  archetypeElementVO.getPath()+"/value/"+attribute;
        ExpressionRuleLineElement expressionRuleLineElement =
                getExpressionRuleLineElement();
        OperatorKind operatorKind =
                getComparisonOperatorRuleLineElement().getValue();
        return new BinaryExpression(
                new Variable(null,archetypeElementVO.getName(), path),
                expressionRuleLineElement.getValue(),
                operatorKind);
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