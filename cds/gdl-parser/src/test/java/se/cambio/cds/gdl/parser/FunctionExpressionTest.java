package se.cambio.cds.gdl.parser;

import se.cambio.cds.gdl.model.expression.ExpressionItem;
import se.cambio.cds.gdl.model.expression.Function;
import se.cambio.cds.gdl.model.expression.FunctionalExpression;

public class FunctionExpressionTest extends ExpressionTestBase {
    public void test_can_parse_natural_logarithm_expression() throws Exception {
        ExpressionItem expressionItem = Expressions.parse("ln($gt0001) ");
        assertTrue("FunctionalExpression expected but got: " + expressionItem.getClass().getSimpleName(),
                expressionItem instanceof FunctionalExpression);
        assertEquals(((FunctionalExpression) expressionItem).getFunction(), Function.LN);
    }

    public void test_can_parse_base_10_logarithm_expression() throws Exception {
        ExpressionItem expressionItem = Expressions.parse("log($gt0001) ");
        assertTrue(expressionItem instanceof FunctionalExpression);
        assertEquals(((FunctionalExpression) expressionItem).getFunction(), Function.LOG);
    }
}
