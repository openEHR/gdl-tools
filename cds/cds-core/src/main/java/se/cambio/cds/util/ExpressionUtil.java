package se.cambio.cds.util;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;
import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.util.export.DVDefSerializer;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.OpenEHRDataValuesUI;

import java.util.Map;
import java.util.Set;

import static java.lang.String.format;
import static org.springframework.util.StringUtils.capitalize;

public class ExpressionUtil {

    public static String CODE_FUNCTION_SEPARATOR = "#";

    public static String getArithmeticExpressionStr(
            Map<String, ArchetypeElementVO> elementMap,
            ExpressionItem expressionItem, Map<RefStat, Set<String>> stats) {
        return getArithmeticExpressionStr(elementMap, expressionItem, stats, null);
    }

    private static String getArithmeticExpressionStr(
            Map<String, ArchetypeElementVO> elementMap,
            ExpressionItem expressionItem, Map<RefStat, Set<String>> stats,
            ExpressionItem parentExpressionItem) {
        StringBuilder sb = new StringBuilder();
        if (expressionItem instanceof BinaryExpression) {
            BinaryExpression binaryExpression = (BinaryExpression) expressionItem;
            if (OperatorKind.EXPONENT.equals(binaryExpression.getOperator())) {
                sb.append("(double) Math.pow(");
                sb.append(getArithmeticExpressionStr(elementMap,
                        binaryExpression.getLeft(), stats, binaryExpression));
                sb.append(",");
                sb.append(getArithmeticExpressionStr(elementMap,
                        binaryExpression.getRight(), stats, binaryExpression));
                sb.append(")");
            } else {
                sb.append("(")
                        .append(getArithmeticExpressionStr(elementMap, binaryExpression.getLeft(), stats, binaryExpression));
                sb.append(binaryExpression.getOperator().getSymbol());
                sb.append(getArithmeticExpressionStr(elementMap, binaryExpression.getRight(), stats, binaryExpression))
                        .append(")");
            }
        } else if (expressionItem instanceof Variable) {
            Variable var = (Variable) expressionItem;
            String rmName = getRMName(elementMap, var);
            sb.append(getVariableWithAttributeStr(rmName, var));
            if (stats != null) {
                if (isFunction(var.getAttribute())) {
                    stats.get(RefStat.ATT_FUNCTIONS).add(var.getCode() + CODE_FUNCTION_SEPARATOR + var.getAttribute());
                    stats.get(RefStat.ATT_FUNCTIONS_REF).add(var.getCode());
                } else {
                    stats.get(RefStat.REFERENCE).add(var.getCode());
                }
            }
        } else if (expressionItem instanceof StringConstant) {
            String stringValue = expressionItem.toString();
            if (stringValue.startsWith("'") && stringValue.endsWith("'") && stringValue.length() > 1) {
                stringValue = "\"" + stringValue.substring(1, stringValue.length() - 1) + "\"";
            }
            sb.append(stringValue);
        } else if (expressionItem instanceof ConstantExpression) {
            sb.append(formatConstantValue(elementMap, (ConstantExpression) expressionItem, parentExpressionItem));
        } else if (expressionItem instanceof FunctionalExpression) {
            FunctionalExpression fe = (FunctionalExpression) expressionItem;
            sb.append("(double)Math.")
                    .append(fe.getFunction().toString())
                    .append("(");
            String postfix = "";
            for (ExpressionItem feItem : fe.getItems()) {
                sb.append(postfix);
                sb.append(getArithmeticExpressionStr(elementMap, feItem, stats, expressionItem));
                postfix = ", ";
            }
            sb.append(")");
        } else {
            throw new RuntimeException(format("Unknown expression '%s'", expressionItem.getClass().getName()));
        }
        return sb.toString();
    }

    private static String getRMName(Map<String, ArchetypeElementVO> elementMap, Variable var) {
        String rmName = null;
        if (OpenEHRConst.CURRENT_DATE_TIME_ID.equals(var.getCode())) {
            rmName = OpenEHRDataValues.DV_DATE_TIME;
        } else {
            ArchetypeElementVO aeVO = elementMap.get(var.getCode());
            if (aeVO == null && !isFunction(var.getAttribute())) {
                throw new RuntimeException(format("Archetype element not found for gtcode '%s'", var.getCode()));
            }
            if (aeVO != null) {
                rmName = aeVO.getRMType();
            }
        }
        return rmName;
    }


    /*
     * Parse for units of hr and convert value to milliseconds
     */
    private static String formatConstantValue(
            Map<String, ArchetypeElementVO> elementMap,
            ConstantExpression exp,
            ExpressionItem parentExpressionItem) {
        String value = exp.getValue();
        if (value.contains(",")) {
            String units = StringUtils.substringAfter(value, ",");
            if (isUcumTime(units)) {
                OperatorKind operatorKind = getOperatorKind(parentExpressionItem);
                if (operatorKind == null) {
                    throw new RuntimeException(format("Operator kind not defined in expression : %s", parentExpressionItem));
                }
                if (hasLeftVariableName(parentExpressionItem)) {
                    String temporalVariableName = getLeftVariableNameFromExpression(elementMap, parentExpressionItem);
                    value = format("DVUtil.calculateDuration(\"%s\",%s,\"%s\")", value, temporalVariableName, operatorKind.getSymbol());
                } else {
                    value = format("DVUtil.calculateDuration(\"%s\",\"%s\")", value, operatorKind.getSymbol());
                }
            } else {
                throw new IllegalArgumentException(format("Unknown time units in value '%s'", value));
            }
        } else if (exp instanceof MathConstant) {
            if (Constant.E.equals(((MathConstant) exp).getConstant())) {
                value = "(double) " + Math.E;
            }
        }
        return "(" + value + ")";
    }

    private static OperatorKind getOperatorKind(ExpressionItem parentExpressionItem) {
        if (parentExpressionItem instanceof BinaryExpression) {
            return ((BinaryExpression) parentExpressionItem).getOperator();
        } else {
            return null;
        }
    }

    private static boolean hasLeftVariableName(
            ExpressionItem parentExpressionItem) {
        return (parentExpressionItem instanceof BinaryExpression) &&
                ((BinaryExpression) parentExpressionItem).getLeft() instanceof Variable;
    }

    private static String getLeftVariableNameFromExpression(
            Map<String, ArchetypeElementVO> elementMap,
            ExpressionItem parentExpressionItem) {
        if (parentExpressionItem instanceof BinaryExpression) {
            ExpressionItem left = ((BinaryExpression) parentExpressionItem).getLeft();
            if (left instanceof Variable) {
                return getLeftVariable(elementMap, (Variable) left);
            }
        }
        throw new RuntimeException(format("Invalid expression %s", parentExpressionItem));
    }

    private static String getLeftVariable(Map<String, ArchetypeElementVO> elementMap, Variable leftVariable) {
        String code = leftVariable.getCode();
        if (OpenEHRConst.CURRENT_DATE_TIME_ID.equals(code)) {
            return "$" + OpenEHRConst.CURRENT_DATE_TIME_ID;
        } else {
            String rmName = getRMName(elementMap, leftVariable);
            String dvClassName = DVDefSerializer.getDVClassName(rmName);
            String variableExp = "$" + code + ".getDataValue()";
            if (leftVariable.getAttribute() != null) {
                variableExp = "((" + dvClassName + ")$" + leftVariable.getCode()
                        + getDataValueMethod(leftVariable.getCode()) + ").get"
                        + capitalize(leftVariable.getAttribute()) + "()";
            }
            return variableExp;
        }
    }

    private static boolean isUcumTime(String ucumUnits) {
        return ucumUnits.equals("a")
                || ucumUnits.equals("mo")
                || ucumUnits.equals("wk")
                || ucumUnits.equals("d")
                || ucumUnits.equals("h")
                || ucumUnits.equals("min")
                || ucumUnits.equals("s")
                || ucumUnits.equals("S");
    }

    public static String getVariableWithAttributeStr(String rmName, Variable var) {
        LoggerFactory.getLogger(DVUtil.class).debug("Var.code: " + var.getCode() + ", attr: " + var.getAttribute());
        String ret = null;
        String dvClassName = null;
        if (rmName != null) {
            dvClassName = DVDefSerializer.getDVClassName(rmName);
        }
        if (OpenEHRConst.CURRENT_DATE_TIME_ID.equals(var.getCode()) && (var.getAttribute() == null || var.getAttribute().equals("value"))) {
            ret = "$" + OpenEHRConst.CURRENT_DATE_TIME_ID + ".getDateTime().getMillis()";
        } else if ("value".equals(var.getAttribute()) && ("DvDateTime".equals(dvClassName) || "DvDate".equals(dvClassName))) {
            ret = "((" + dvClassName + ")$" + var.getCode() + getDataValueMethod(var.getCode()) + ").getDateTime().getMillis()";
        } else {
            if (isFunction(var.getAttribute())) {
                //Function (Only working for count yet)
                if (OpenEHRDataValues.FUNCTION_COUNT.equals(var.getAttribute())) {
                    ret = "$" + var.getCode() + var.getAttribute();
                }
            } else {
                if (var.getAttribute() != null) {
                    //Attribute
                    ret = "((" + dvClassName + ")$" + var.getCode()
                            + getDataValueMethod(var.getCode()) + ").get"
                            + StringUtils.capitalize(var.getAttribute()) + "()";
                } else {
                    ret = "DVUtil.ucumToMilliseconds((" + dvClassName + ")$" + var.getCode()
                            + getDataValueMethod(var.getCode()) + ")";
                }
            }
        }
        return ret;
    }

    public static String getDataValueMethod(String gtCode) {
        if (!OpenEHRConst.CURRENT_DATE_TIME_ID.equals(gtCode)) {
            return ".getDataValue()";
        } else {
            return "";
        }
    }

    public static boolean isFunction(String attribute) {
        return OpenEHRDataValuesUI.getFunctionNames().contains(attribute);
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