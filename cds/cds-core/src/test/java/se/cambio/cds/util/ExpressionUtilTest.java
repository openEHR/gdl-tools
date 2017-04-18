package se.cambio.cds.util;

import org.testng.annotations.Test;
import se.cambio.cds.gdl.model.expression.BinaryExpression;
import se.cambio.cds.gdl.model.expression.ConstantExpression;
import se.cambio.cds.gdl.model.expression.ExpressionItem;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

public class ExpressionUtilTest {
    @Test
    public void testGetArithmeticExpressionStrForComplexDurationOperation() throws Exception {
        Map<String, ArchetypeElementVO> elementMap = new HashMap<>();
        Map<RefStat, Set<String>> stats = new HashMap<>();
        ConstantExpression constantExpression = new ConstantExpression("1,d");
        ExpressionItem expressionItem = new BinaryExpression(new BinaryExpression(constantExpression, constantExpression, OperatorKind.ADDITION), constantExpression, OperatorKind.ADDITION);
        String arithmeticExpressionStr = ExpressionUtil.getArithmeticExpressionStr(elementMap, expressionItem, stats);
        assertThat(arithmeticExpressionStr, equalTo("(((DVUtil.calculateDuration(\"1,d\",\"+\"))+(DVUtil.calculateDuration(\"1,d\",\"+\")))+(DVUtil.calculateDuration(\"1,d\",\"+\")))"));
    }

}