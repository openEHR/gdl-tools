package se.cambio.cds.gdl.model.readable.rule.lines;

import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.*;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ArchetypeElementRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.DefinitionsRuleLine;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRLanguageManager;


public class WithElementPredicateExistsDefinitionRuleLine extends ExpressionRuleLine implements ArchetypeElementRuleLine, DefinitionsRuleLine{

    private ArchetypeElementRuleLineDefinitionElement archetypeElementRuleLineDefinitionElement = null;
    private ExistenceOperatorRuleLineElement existenceOperatorRuleLineElement = null;
    private String NULL_STR = "null";


    public WithElementPredicateExistsDefinitionRuleLine() {
        super(OpenEHRLanguageManager.getMessage("ElementPredicateExists"),
                OpenEHRLanguageManager.getMessage("ElementPredicateExistsDesc"));
        archetypeElementRuleLineDefinitionElement = new ArchetypeElementRuleLineDefinitionElement(this);
        existenceOperatorRuleLineElement = new ExistenceOperatorRuleLineElement(this);
        getRuleLineElements().add(new StaticTextRuleLineElement(OpenEHRLanguageManager.getMessage("WithElementRLE")));
        getRuleLineElements().add(archetypeElementRuleLineDefinitionElement);
        getRuleLineElements().add(existenceOperatorRuleLineElement);
    }

    public ArchetypeElementRuleLineDefinitionElement getArchetypeElementRuleLineDefinitionElement() {
        return archetypeElementRuleLineDefinitionElement;
    }

    @Override
    public ArchetypeReference getArchetypeReference() {
        return getArchetypeInstantiationRuleLine().
                getArchetypeReferenceRuleLineDefinitionElement().getValue();
    }

    @Override
    public ArchetypeElementVO getArchetypeElement() {
        return archetypeElementRuleLineDefinitionElement.getValue();
    }

    public ExistenceOperatorRuleLineElement getExistenceOperatorRuleLineElement(){
        return existenceOperatorRuleLineElement;
    }

    public ArchetypeInstantiationRuleLine getArchetypeInstantiationRuleLine() {
        return (ArchetypeInstantiationRuleLine)getParentRuleLine();
    }

    public ExpressionItem toExpressionItem() throws IllegalStateException{
        ArchetypeElementVO archetypeElementVO = getArchetypeElement();
        if (archetypeElementVO!=null){
            String path =  archetypeElementVO.getPath();
            OperatorKind operatorKind = getExistenceOperatorRuleLineElement().getOperator();
            if (operatorKind==null){
                throw new IllegalStateException("No operator set");
            }
            return new BinaryExpression(
                    new Variable(null, archetypeElementVO.getName(), path),
                    new ConstantExpression(NULL_STR),
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