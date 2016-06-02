package se.cambio.cds.gdl.parser;

import se.cambio.cds.gdl.model.expression.ExpressionItem;
import se.cambio.cds.gdl.model.expression.MathConstant;

public class MathConstantExpressionTest extends ExpressionTestBase {
    public void test_can_parse_expression_with_number_e() throws Exception {
        ExpressionItem expressionItem = Expressions.parse("e");
        assertTrue(expressionItem instanceof MathConstant);
    }

    // e^(X–0.0158×Age+0.438×ln(Age))
    public void disabled_test_can_parse_assignment_expression_complex_algorithm_including_number_e() throws Exception {
        //ExpressionItem expressionItem = Expressions.parse("(e^(($gt0001–(0.0158*($gt0002+(0.438×(ln($gt0003)))))))");
        ExpressionItem expressionItem = Expressions.parse("(e^$gt0001)");
        assertTrue(expressionItem instanceof MathConstant);
    }
}
