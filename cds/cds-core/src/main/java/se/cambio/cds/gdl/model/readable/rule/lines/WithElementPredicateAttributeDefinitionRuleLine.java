package se.cambio.cds.gdl.model.readable.rule.lines;

import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeElementRuleLineDefinitionElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.DataValueRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.PredicateComparisonOperatorRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.StaticTextRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ArchetypeElementRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.DefinitionsRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.PredicateRuleLine;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.util.DVUtil;
import se.cambio.cds.util.export.DVDefSerializer;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.util.UserConfigurationManager;


public class WithElementPredicateAttributeDefinitionRuleLine extends ExpressionRuleLine implements ArchetypeElementRuleLine, DefinitionsRuleLine, PredicateRuleLine{

    private ArchetypeElementRuleLineDefinitionElement archetypeElementRuleLineDefinitionElement = null;
    private PredicateComparisonOperatorRuleLineElement comparisonOperatorRuleLineElement = null;
    private DataValueRuleLineElement dataValueRuleLineElement = null;


    public WithElementPredicateAttributeDefinitionRuleLine() {
        super(OpenEHRLanguageManager.getMessage("ElementPredicateAttribute"),
                OpenEHRLanguageManager.getMessage("ElementPredicateAttributeDesc"));
        archetypeElementRuleLineDefinitionElement = new ArchetypeElementRuleLineDefinitionElement(this);
        comparisonOperatorRuleLineElement = new PredicateComparisonOperatorRuleLineElement(this);
        dataValueRuleLineElement = new DataValueRuleLineElement(this);

        getRuleLineElements().add(new StaticTextRuleLineElement("WithElementRLE"));
        getRuleLineElements().add(archetypeElementRuleLineDefinitionElement);
        getRuleLineElements().add(comparisonOperatorRuleLineElement);
        getRuleLineElements().add(dataValueRuleLineElement);
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

    public PredicateComparisonOperatorRuleLineElement getComparisonOperatorRuleLineElement() {
        return comparisonOperatorRuleLineElement;
    }

    public DataValueRuleLineElement getDataValueRuleLineElement() {
        return dataValueRuleLineElement;
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
        String name = getArchetypeManager().getArchetypeElements().getText(archetypeElementVO, UserConfigurationManager.getLanguage());
        return new BinaryExpression(
                new Variable(null,name, path),
                constantExpression,
                operatorKind);
    }

    @Override
    public String getPredicateDescription() {
        StringBuilder sb = new StringBuilder();
        ArchetypeElementRuleLineDefinitionElement aerlde = getArchetypeElementRuleLineDefinitionElement();
        if (aerlde!=null){
            ArchetypeElementVO archetypeElementVO = aerlde.getValue();
            if (archetypeElementVO!=null){
                String name = aerlde.getArchetypeManager().getArchetypeElements().getText(archetypeElementVO, UserConfigurationManager.getLanguage());
                sb.append(name+"="+ DVDefSerializer.getReadableValue(getDataValueRuleLineElement().getValue(), null));
            }else{
                Logger.getLogger(ArchetypeReference.class).warn("Unknown predicate for AR '"+aerlde.toString()+"'");
                sb.append("*UNKNOWN PREDICATE*");
            }
        }
        return sb.toString();
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