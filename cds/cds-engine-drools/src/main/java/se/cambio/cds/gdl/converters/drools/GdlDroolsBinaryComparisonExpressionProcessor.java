package se.cambio.cds.gdl.converters.drools;

import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.cds.gdl.model.expression.BinaryExpression;
import se.cambio.cds.gdl.model.expression.ConstantExpression;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.gdl.model.expression.Variable;
import se.cambio.cds.util.ExpressionUtil;
import se.cambio.cds.util.RefStat;
import se.cambio.cds.util.export.DVDefSerializer;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRDataValues;

import java.util.Map;
import java.util.Set;

import static java.lang.String.format;

public class GdlDroolsBinaryComparisonExpressionProcessor {

    private final GDLDroolsConverter gdlDroolsConverter;
    private final BinaryExpression binaryExpression;
    private final Map<RefStat, Set<String>> stats;
    private StringBuffer stringBuffer;

    public GdlDroolsBinaryComparisonExpressionProcessor(GDLDroolsConverter gdlDroolsConverter, BinaryExpression binaryExpression, Map<RefStat, Set<String>> stats) {
        this.gdlDroolsConverter = gdlDroolsConverter;
        this.binaryExpression = binaryExpression;
        this.stats = stats;
        this.stringBuffer = new StringBuffer();
    }

    public String process() {
        processComparisonExpression();
        return stringBuffer.toString();
    }

    protected void processComparisonExpression() {
        Variable var = null;
        if (binaryExpression.getLeft() instanceof Variable) {
            var = (Variable) binaryExpression.getLeft();
            stats.get(RefStat.REFERENCE).add(var.getCode());
        }
        if (var != null) {
            ArchetypeElementVO archetypeElementVO = gdlDroolsConverter.getElementMap().get(var.getCode());
            if (var.getAttribute() == null) {
                processVariableWithoutAttribute(var, archetypeElementVO);
            } else {
                processVariableWithAttribute(var, archetypeElementVO);
            }
        } else {
            throw new RuntimeException(format("Unknown expression '%s'", binaryExpression.getLeft()));
        }
    }

    private void processVariableWithAttribute(Variable var, ArchetypeElementVO archetypeElementVO) {
        if (var.getAttribute().equals(OpenEHRConst.NULL_FLAVOR_ATTRIBUTE)) {
            processNullFlavour(var);
        } else {
            processVariableWithAttributeNotNull(var, archetypeElementVO);
        }
    }

    private void processVariableWithAttributeNotNull(Variable var, ArchetypeElementVO archetypeElementVO) {
        Map<RefStat, Set<String>> statsAux = gdlDroolsConverter.initStats();
        String arithmeticExpStr = ExpressionUtil.getArithmeticExpressionStr(gdlDroolsConverter.getElementMap(), binaryExpression.getRight(), statsAux);
        ExpressionUtil.getArithmeticExpressionStr(gdlDroolsConverter.getElementMap(), binaryExpression.getLeft(), statsAux);
        stats.get(RefStat.REFERENCE).addAll(statsAux.get(RefStat.REFERENCE));
        stats.get(RefStat.ATT_FUNCTIONS).addAll(statsAux.get(RefStat.ATT_FUNCTIONS));

        statsAux.get(RefStat.REFERENCE).remove(OpenEHRConst.CURRENT_DATE_TIME_ID);
        stringBuffer.append("eval(");
        for (String gtCode : statsAux.get(RefStat.REFERENCE)) {
            stringBuffer.append("$" + gtCode + ".hasValue() && ");
        }
        String rmName = null;
        if (archetypeElementVO != null) {
            rmName = archetypeElementVO.getType();
        }
        stringBuffer.append("(");
        String varCall = ExpressionUtil.getVariableWithAttributeStr(rmName, var);
        if (rmName != null && gdlDroolsConverter.isString(rmName, var.getAttribute())) {
            stringBuffer.append(gdlDroolsConverter.getAttributeOperatorMVELLine(varCall, binaryExpression.getOperator(), arithmeticExpStr));
        } else {
            stringBuffer.append(varCall);
            stringBuffer.append(binaryExpression.getOperator().getSymbol());
            stringBuffer.append(arithmeticExpStr);
        }
        stringBuffer.append("))");
    }

    private void processNullFlavour(Variable var) {
        ConstantExpression constantExpression = (ConstantExpression) binaryExpression.getRight();
        String dvStr = constantExpression.getValue();
        DataValue dv = DataValue.parseValue(OpenEHRDataValues.DV_CODED_TEXT + "," + dvStr);
        stringBuffer.append("eval(");
        String opNeg = (binaryExpression.getOperator().equals(OperatorKind.INEQUAL)) ? "!" : "";
        stringBuffer.append(opNeg + "DVUtil.nullValueEquals($" + var.getCode() + ".getNullFlavour(), " + DVDefSerializer.getDVInstantiation(dv) + "))");
    }

    private void processVariableWithoutAttribute(Variable var, ArchetypeElementVO archetypeElementVO) {
        if (binaryExpression.getRight() instanceof ConstantExpression) {
            processVariableWithoutAttributeConstantExpression(var, archetypeElementVO);
        } else if (binaryExpression.getRight() instanceof Variable) {
            processVariableWithoutAttributeVariable(var);
        } else {
            throw new RuntimeException(format("Unknown expression '%s'", binaryExpression.getRight().getClass().getName()));
        }
    }

    private void processVariableWithoutAttributeVariable(Variable var) {
        Variable varRight = (Variable) binaryExpression.getRight();
        String gtCodeAux = varRight.getCode();
        stringBuffer.append("eval($" + var.getCode() + ".hasValue() && ");
        stringBuffer.append("$" + gtCodeAux + ".hasValue() && ");
        stringBuffer.append(gdlDroolsConverter.getOperatorMVELLine("$" + var.getCode(), binaryExpression.getOperator(), "$" + gtCodeAux));
        stringBuffer.append(")");
        stats.get(RefStat.REFERENCE).add(gtCodeAux);
    }

    private void processVariableWithoutAttributeConstantExpression(Variable var, ArchetypeElementVO archetypeElementVO) {
        ConstantExpression constantExpression = (ConstantExpression) binaryExpression.getRight();
        String dvStr = constantExpression.getValue();
        DataValue dv = null;
        if (!dvStr.equals("null")) {
            if (archetypeElementVO == null) {
                String guideId = gdlDroolsConverter.getGuide().getId();
                throw new RuntimeException(format("Element '%s' not found. (guideId='%s')", var.getCode(), guideId));
            }
            String rmType = archetypeElementVO.getType();
            dv = DataValue.parseValue(rmType + "," + dvStr);
        }
        if (dv != null) {
            stringBuffer.append("eval(");
            if (!OpenEHRConst.CURRENT_DATE_TIME_ID.equals(var.getCode())) {
                stringBuffer.append("$").append(var.getCode()).append(".hasValue() && ");
            }
            stringBuffer.append(gdlDroolsConverter.getOperatorMVELLine("$" + var.getCode(),
                    binaryExpression.getOperator(),
                    DVDefSerializer.getDVInstantiation(dv)));
            stringBuffer.append(")");
        } else {
            if (OperatorKind.EQUALITY.equals(binaryExpression.getOperator())) {
                String guideId = gdlDroolsConverter.getGuide().getId();
                stringBuffer.append("eval($" + var.getCode() + ".hasNoValue(\"" + guideId + "/" + var.getCode() + "\"))");
            } else if (OperatorKind.INEQUAL.equals(binaryExpression.getOperator())) {
                stringBuffer.append("eval($" + var.getCode() + ".hasValue())");
            }
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