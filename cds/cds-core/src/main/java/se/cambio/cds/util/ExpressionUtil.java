package se.cambio.cds.util;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import se.cambio.cds.gdl.model.expression.*;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
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
                sb.append("Math.pow(");
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
            if (OpenEHRConst.CURRENT_DATE_TIME_ID.equals(var.getCode())){
                rmName = OpenEHRDataValues.DV_DATE_TIME;
            }else{
                ArchetypeElementVO aeVO = elementMap.get(var.getCode());
                if (aeVO==null){
                    throw new InternalErrorException(new Exception("Archetype element not found for gtcode '"+var.getCode()+"'"));
                }
                rmName = aeVO.getRMType();
            }
            sb.append(getVariableWithAttributeStr(rmName, var));
            if (stats!=null){
                if (isFunction(var.getAttribute())){
                    stats.get(RefStat.ATT_FUNCTIONS).add(var.getCode()+CODE_FUNCTION_SEPARATOR+var.getAttribute());
                    stats.get(RefStat.ATT_FUNCTIONS_REF).add(var.getCode());
                }else{
                    stats.get(RefStat.REFERENCE).add(var.getCode());
                }
            }
        } else if (expressionItem instanceof StringConstant) {
            sb.append(expressionItem.toString());
        } else if (expressionItem instanceof ConstantExpression) {
            sb.append(formatConstantValue((ConstantExpression) expressionItem));
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
    public static String formatConstantValue(ConstantExpression exp) {
        String value = exp.getValue();
        int i = value.indexOf(",");
        if(i > 0 ){
            //Convert time units to milliseconds
            String units = value.substring(i+1).trim();
            if (units.equals("a")) {
                double d = Double.parseDouble(value.substring(0, i));
                value = "31556926000L*" + d;
            }else if (units.equals("mo")) {
                double d = Double.parseDouble(value.substring(0, i));
                value = "2629743830L*" + d;
            }else if (units.equals("wk")) {
                double d = Double.parseDouble(value.substring(0, i));
                value = "604800000L*" + d;
            }else if (units.equals("d")) {
                double d = Double.parseDouble(value.substring(0, i));
                value = "86400000L*" + d;
            }else if (units.equals("h")) {
                double d = Double.parseDouble(value.substring(0, i));
                value = "3600000L*" + d;
            }else if (units.equals("min")) {
                double d = Double.parseDouble(value.substring(0, i));
                value = "60000L*" + d;
            }else if (units.equals("s")) {
                double d = Double.parseDouble(value.substring(0, i));
                value = "1000L*" + d;
            } else { //milliseconds or any other unit
                double d = Double.parseDouble(value.substring(0, i));
                value = "" + d;
            }
        }
        return value;
    }

    public static String getVariableWithAttributeStr(String rmName, Variable var) {

        Logger.getLogger(DVUtil.class).debug("Var.code: " + var.getCode() + ", attr: " + var.getAttribute());

        String dvClassName = DVDefSerializer.getDVClassName(rmName);
        String ret = null;

        // TODO fix setting currentDateTime
        if(OpenEHRConst.CURRENT_DATE_TIME_ID.equals(var.getCode()) && (var.getAttribute()==null||var.getAttribute().equals("value"))) {
            ret = "$"+OpenEHRConst.CURRENT_DATE_TIME_ID+".getDateTime().getMillis()";
        } else if("value".equals(var.getAttribute()) &&("DvDateTime".equals(dvClassName)
                || "DvDate".equals(dvClassName))) {
            ret = "((" + dvClassName + ")$" + var.getCode() +
                    getDataValueMethod(var.getCode()) +
                    ").getDateTime().getMillis()";
        } else {
            if (isFunction(var.getAttribute())){
                //Function (Only working for count yet)
                if (OpenEHRDataValues.FUNCTION_COUNT.equals(var.getAttribute())){
                    ret = "$"+var.getCode()+var.getAttribute();
                }
            }else{
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

    public static boolean isFunction(String attribute){
        return OpenEHRDataValuesUI.getFunctionNames().contains(attribute);
    }
}
