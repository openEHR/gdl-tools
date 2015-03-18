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
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Map;
import java.util.Set;

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

    public String process() throws InternalErrorException {
        processComparisonExpression();
        return stringBuffer.toString();
    }

    protected void processComparisonExpression() throws InternalErrorException {
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
            throw new InternalErrorException(new Exception("Unknown expression '" + binaryExpression.getLeft() + "'"));
        }
    }

    private void processVariableWithAttribute(Variable var, ArchetypeElementVO archetypeElementVO) throws InternalErrorException {
        if (var.getAttribute().equals(OpenEHRConst.NULL_FLAVOR_ATTRIBUTE)){
            processNullFlavour(var);
        }else{
            processVariableWithAttributeNotNull(var, archetypeElementVO);
        }
    }

    private void processVariableWithAttributeNotNull(Variable var, ArchetypeElementVO archetypeElementVO) throws InternalErrorException {
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
            rmName = archetypeElementVO.getRMType();
        }
        stringBuffer.append("(");
        String varCall = ExpressionUtil.getVariableWithAttributeStr(rmName, var);
        if (rmName != null && gdlDroolsConverter.isString(rmName, var.getAttribute())){
            stringBuffer.append(gdlDroolsConverter.getAttributeOperatorMVELLine(varCall, binaryExpression.getOperator(), arithmeticExpStr));
        }else{
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

    private void processVariableWithoutAttribute(Variable var, ArchetypeElementVO archetypeElementVO) throws CompilationErrorException {
        if (binaryExpression.getRight() instanceof ConstantExpression) {
            processVariableWithoutAttributeConstantExpression(var, archetypeElementVO);
        } else if (binaryExpression.getRight() instanceof Variable) {
            processVariableWithoutAttributeVariable(var);
        } else {
            throw new CompilationErrorException("Unknown expression '" + binaryExpression.getRight().getClass().getName() + "'");
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

    private void processVariableWithoutAttributeConstantExpression(Variable var, ArchetypeElementVO archetypeElementVO) throws CompilationErrorException {
        ConstantExpression constantExpression = (ConstantExpression) binaryExpression.getRight();
        String dvStr = constantExpression.getValue();
        DataValue dv = null;
        if (!dvStr.equals("null")) {
            if (archetypeElementVO == null){
                String guideId = gdlDroolsConverter.getGuide().getId();
                throw new CompilationErrorException("Element '" + var.getCode() + "' not found. (guideId='" + guideId + "')");
            }
            String rmType = archetypeElementVO.getRMType();
            dv = DataValue.parseValue(rmType + "," + dvStr);
        }
        if (dv != null) {
            stringBuffer.append("eval(");
            if (!OpenEHRConst.CURRENT_DATE_TIME_ID.equals(var.getCode())){
                stringBuffer.append("$" + var.getCode() + ".hasValue() && ");
            }
            stringBuffer.append(gdlDroolsConverter.getOperatorMVELLine("$" + var.getCode(),
                    binaryExpression.getOperator(),
                    DVDefSerializer.getDVInstantiation(dv)));
            stringBuffer.append(")");
        } else {
            if (OperatorKind.EQUALITY.equals(binaryExpression.getOperator())) {
                String guideId = gdlDroolsConverter.getGuide().getId();
                stringBuffer.append("eval($" + var.getCode() + ".hasNoValue(\"" + guideId + "/"+var.getCode() + "\"))");
            } else if (OperatorKind.INEQUAL.equals(binaryExpression.getOperator())) {
                stringBuffer.append("eval($" + var.getCode() + ".hasValue())");
            }
        }
    }
}
