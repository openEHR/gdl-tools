package se.cambio.cds.gdl.model.readable.util;

import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.RuleLineElementWithValue;

public class ExpressionUtil {

    public static String convertToHTMLText(RuleLineElementWithValue<ExpressionItem> ruleLineElement, ExpressionItem expressionItem, String language) {
        StringBuffer sb = new StringBuffer();
        sb.append(toString(ruleLineElement, expressionItem, language));
        return sb.toString();
    }

    public static String toString(RuleLineElementWithValue<ExpressionItem> ruleLineElement, ExpressionItem expressionItem, String language) {
        if (expressionItem instanceof Variable) {
            return getVariableName(ruleLineElement, (Variable) expressionItem, language);
        } else if (expressionItem instanceof BinaryExpression) {
            BinaryExpression be = (BinaryExpression) expressionItem;
            return "(" + toString(ruleLineElement, be.getLeft(), language) + " " + be.getOperator().getSymbol() + " " + toString(ruleLineElement, be.getRight(), language) + ")";
        } else if (expressionItem instanceof UnaryExpression) {
            UnaryExpression ue = (UnaryExpression) expressionItem;
            return ue.getOperator().getSymbol() + "(" + toString(ruleLineElement, ue.getOperand(), language) + ")";
        } else if (expressionItem instanceof FunctionalExpression) {
            FunctionalExpression fe = (FunctionalExpression) expressionItem;
            StringBuilder sb = new StringBuilder();
            String postFix = "";
            for(ExpressionItem ei : fe.getItems()) {
                sb.append(postFix)
                        .append(toString(ruleLineElement, ei, language));
                postFix = ",";
            }
            return "(" + fe.getFunction().toString() + "(" + sb.toString() + "))";
        } else if (expressionItem instanceof StringConstant) {
            return expressionItem.toString();
        } else {
            return expressionItem != null ? expressionItem.toString().replace(",", " ") : null;
        }
    }

    public static String getVariableName(RuleLineElementWithValue<ExpressionItem> ruleLineElement, Variable var, String language) {
        String attStr = "";
        if (var.getAttribute() != null) {
            if (!"magnitude".equals(var.getAttribute()) && !"value".equals(var.getAttribute())) {
                attStr = "<font size=2><sub>" + var.getAttribute().toUpperCase() + "</sub></font>";
            }
        }
        return "<b>" + ruleLineElement.getName(var.getCode(), language) + attStr + "</b>";
    }

    public static String getEditableExpressionString(ExpressionItem expressionItem) {
        if (expressionItem instanceof Variable) {
            Variable var = (Variable) expressionItem;
            String attributePostfix = var.getAttribute() != null ? "." + var.getAttribute() : "";
            return "$" + var.getCode() + attributePostfix;
        } else if (expressionItem instanceof BinaryExpression) {
            BinaryExpression be = (BinaryExpression) expressionItem;
            return "(" + getEditableExpressionString(be.getLeft()) + " " + be.getOperator().getSymbol() + " " + getEditableExpressionString(be.getRight()) + ")";
        } else if (expressionItem instanceof UnaryExpression) {
            UnaryExpression ue = (UnaryExpression) expressionItem;
            return ue.getOperator().getSymbol() + "(" + getEditableExpressionString(ue.getOperand()) + ")";
        } else if (expressionItem instanceof FunctionalExpression) {
            FunctionalExpression fe = (FunctionalExpression) expressionItem;
            StringBuilder sb = new StringBuilder();
            String postFix = "";
            for(ExpressionItem ei : fe.getItems()) {
                sb.append(postFix)
                        .append(getEditableExpressionString(ei));
                postFix = ", ";
            }
            return "(" + fe.getFunction().toString() + "(" + sb.toString() + "))";
        } else {
            return expressionItem.toString();
        }
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