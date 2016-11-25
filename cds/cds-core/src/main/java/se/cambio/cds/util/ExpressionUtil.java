package se.cambio.cds.util;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.util.export.DVDefSerializer;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.OpenEHRDataValuesUI;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Map;
import java.util.Set;

/**
 * User: iago.corbal
 * Date: 2013-12-11
 * Time: 17:23
 */
public class ExpressionUtil {

    public static String CODE_FUNCTION_SEPARATOR = "#";

    public static String getArithmeticExpressionStr(
            Map<String, ArchetypeElementVO> elementMap,
            ExpressionItem expressionItem, Map<RefStat, Set<String>> stats) throws InternalErrorException {
        StringBuffer sb = new StringBuffer();
        if (expressionItem instanceof BinaryExpression) {
            BinaryExpression binaryExpression = (BinaryExpression) expressionItem;
            if (OperatorKind.EXPONENT.equals(binaryExpression.getOperator())) {
                sb.append("(double) Math.pow(");
                sb.append(getArithmeticExpressionStr(elementMap,
                        binaryExpression.getLeft(), stats));
                sb.append(",");
                sb.append(getArithmeticExpressionStr(elementMap,
                        binaryExpression.getRight(), stats));
                sb.append(")");
            } else {
                sb.append("("
                        + getArithmeticExpressionStr(elementMap,
                        binaryExpression.getLeft(), stats));
                sb.append(binaryExpression.getOperator().getSymbol());
                sb.append(getArithmeticExpressionStr(elementMap,
                        binaryExpression.getRight(), stats) + ")");
            }
        } else if (expressionItem instanceof Variable) {
            Variable var = (Variable) expressionItem;
            String rmName = null;
            if (OpenEHRConst.CURRENT_DATE_TIME_ID.equals(var.getCode())) {
                rmName = OpenEHRDataValues.DV_DATE_TIME;
            } else {
                ArchetypeElementVO aeVO = elementMap.get(var.getCode());
                if (aeVO == null && !isFunction(var.getAttribute())) {
                    throw new InternalErrorException(new Exception("Archetype element not found for gtcode '" + var.getCode() + "'"));
                }
                if (aeVO != null) {
                    rmName = aeVO.getRMType();
                }
            }
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
            sb.append(formatConstantValue((ConstantExpression) expressionItem));
        } else if (expressionItem instanceof FunctionalExpression) {
            FunctionalExpression fe = (FunctionalExpression) expressionItem;
            sb.append("(double) Math." + fe.getFunction().toString()).append("(");
            String postfix = "";
            for (ExpressionItem feItem : fe.getItems()) {
                sb.append(postfix);
                sb.append(getArithmeticExpressionStr(elementMap, feItem, stats));
                postfix = ", ";
            }
            sb.append(")");
        } else {
            throw new InternalErrorException(new Exception(
                    "Unknown expression '"
                            + expressionItem.getClass().getName() + "'"));
        }
        return sb.toString();
    }


    /*
     * Parse for units of hr and convert value to milliseconds
     */
    private static String formatConstantValue(ConstantExpression exp) throws InternalErrorException {
        String value = exp.getValue();
        int i = value.indexOf(",");
        if (i > 0) {
            //Convert time units to milliseconds
            String units = value.substring(i + 1).trim();
            if (units.equals("a")) {
                double d = Double.parseDouble(value.substring(0, i));
                value = "31556926000L*" + d;
            } else if (units.equals("mo")) {
                double d = Double.parseDouble(value.substring(0, i));
                value = "2629743830L*" + d;
            } else if (units.equals("wk")) {
                double d = Double.parseDouble(value.substring(0, i));
                value = "604800000L*" + d;
            } else if (units.equals("d")) {
                double d = Double.parseDouble(value.substring(0, i));
                value = "86400000L*" + d;
            } else if (units.equals("h")) {
                double d = Double.parseDouble(value.substring(0, i));
                value = "3600000L*" + d;
            } else if (units.equals("min")) {
                double d = Double.parseDouble(value.substring(0, i));
                value = "60000L*" + d;
            } else if (units.equals("s")) {
                double d = Double.parseDouble(value.substring(0, i));
                value = "1000L*" + d;
            } else if (units.equals("S")) {
                double d = Double.parseDouble(value.substring(0, i));
                value = "(long) " + d;
            } else {
                throw new InternalErrorException(new Exception("Unknown time units '" + units + "'"));
            }
        } else if (exp instanceof MathConstant) {
            if (Constant.E.equals(((MathConstant) exp).getConstant())) {
                value = "(double) " + Math.E;
            }
        }
        return "(" + value + ")";
    }

    public static String getVariableWithAttributeStr(String rmName, Variable var) {
        Logger.getLogger(DVUtil.class).debug("Var.code: " + var.getCode() + ", attr: " + var.getAttribute());
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
                //Attribute
                ret = "((" + dvClassName + ")$" + var.getCode()
                        + getDataValueMethod(var.getCode()) + ").get"
                        + StringUtils.capitalize(var.getAttribute()) + "()";
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