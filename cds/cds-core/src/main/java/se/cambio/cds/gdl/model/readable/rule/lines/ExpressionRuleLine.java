package se.cambio.cds.gdl.model.readable.rule.lines;

import org.apache.log4j.Logger;
import se.cambio.cds.gdl.model.expression.BinaryExpression;
import se.cambio.cds.gdl.model.expression.ExpressionItem;
import se.cambio.cds.gdl.model.expression.OperatorKind;

import java.util.List;

public abstract class ExpressionRuleLine extends RuleLine{

    public ExpressionRuleLine(String name, String description) {
        super(name, description);
    }

    public abstract ExpressionItem toExpressionItem() throws IllegalStateException;

    protected ExpressionItem convertToExpressionItem(List<RuleLine> ruleLines, int index) throws IllegalStateException{
        if (ruleLines!=null && ruleLines.size()>index){
            ExpressionRuleLine expressionRuleLine = (ExpressionRuleLine)ruleLines.get(index);
            if (expressionRuleLine!=null){
                ExpressionItem expressionItem = expressionRuleLine.toExpressionItem();
                if (index==ruleLines.size()-1){
                    return expressionItem;
                }else{
                    return new BinaryExpression(expressionItem, convertToExpressionItem(ruleLines, ++index), OperatorKind.AND);
                }
            }
        }
        Logger.getLogger(ElementAttributeComparisonConditionRuleLine.class).warn("Element instance not found for"+ this.toString());
        return null;

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