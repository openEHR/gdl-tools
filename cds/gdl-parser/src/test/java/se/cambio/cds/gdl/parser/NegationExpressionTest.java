package se.cambio.cds.gdl.parser;

import se.cambio.cds.gdl.model.expression.*;

public class NegationExpressionTest extends ExpressionTestBase {

    public void test_can_parse_simple_negation_expression() throws Exception {
        BinaryExpression binaryExpression = new BinaryExpression(Variable.createByCode("gt0003"),
                StringConstant.create("3"), OperatorKind.GREATER_THAN);
        UnaryExpression expected = new UnaryExpression(binaryExpression, OperatorKind.NOT);
        ExpressionItem expressionItem = Expressions.parse("!($gt0003>3)");
        assertEquals(expected, expressionItem);
    }

    public void test_can_parse_complex_negation_expression_with_nested_binary_expression() throws Exception {
        ExpressionItem expressionItem = Expressions.parse("!($gt0009.value>($gt0007.value-7,a))");
        assertTrue(expressionItem instanceof UnaryExpression);
        assertEquals(((UnaryExpression) expressionItem).getOperator(), OperatorKind.NOT);
    }
}
