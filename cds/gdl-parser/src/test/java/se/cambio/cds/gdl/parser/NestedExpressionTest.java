package se.cambio.cds.gdl.parser;

import se.cambio.cds.gdl.model.expression.*;

public class NestedExpressionTest extends ExpressionTestBase {

	public void testNestedBooleanExpressions() throws Exception {
		parseSingleBooleanExpression("($gt0007 != null) and ($gt0008 != null)");
		Variable var1 = new Variable("gt0007");
		ConstantExpression const1 = new ConstantExpression("null");
		BinaryExpression be1 = new BinaryExpression(var1, const1,
				OperatorKind.INEQUAL);
		Variable var2 = new Variable("gt0008");
		ConstantExpression const2 = new ConstantExpression("null");
		BinaryExpression be2 = new BinaryExpression(var2, const2,
				OperatorKind.INEQUAL);
		expected = new BinaryExpression(be1, be2, OperatorKind.AND);
		check();
	}

	public void testNestedArithmeticExpressions() throws Exception {
		parseSingleExpression("(42.5*$gt0020)/$gt0009");
		check("42.5", "/left/left/value");
		check(OperatorKind.MULTIPLICATION, "/left/operator");
	}

	public void testNestedComplexArithmeticExpressions() throws Exception {
		parseSingleExpression(E1);
		check("42.5", "/left/left/left/value");
		check("gt0020", "/left/left/right/code");
		check("height", "/left/left/right/name");
		check(OperatorKind.DIVISION, "/left/operator");
		check("gt0009", "/left/right/code");
		check("creatine", "/left/right/name");
		check(OperatorKind.MULTIPLICATION, "/operator");
		check("gt0007", "/right/left/left/code");
		check("weight", "/right/left/left/name");
		check(OperatorKind.DIVISION, "/right/left/operator");
		check("70", "/right/left/right/value");
		check(OperatorKind.EXPONENT, "/right/operator");
		check("0.7", "right/right/value");
	}
	
	public void testComplexExpressionTwo() throws Exception {
		parseSingleExpression(E2);
		check("1.23", "/left/left/left/value");
		check(OperatorKind.MULTIPLICATION, "/left/left/operator");
		check("140", "/left/left/right/left/value");
		check(OperatorKind.SUBSTRATION, "/left/left/right/operator");
		check("gt0004", "/left/left/right/right/code");
		check("age", "/left/left/right/right/name");
		check(OperatorKind.MULTIPLICATION, "/left/operator");
		check("gt0007", "/left/right/code");
		check("weight", "/left/right/name");
		check(OperatorKind.DIVISION, "/operator");
		check("gt0009", "/right/code");
		check("creatine", "/right/name");	
	}
	
	public void testNestedComplexArithmeticExpressions3() throws Exception {
		parseSingleExpression(E3);
		check("42.5", "/left/left/left/value");
		check("gt0007", "/left/left/right/code");
		check("magnitude", "/left/left/right/attribute");
		check(OperatorKind.DIVISION, "/left/operator");
		check("gt0009", "/left/right/code");
		check("magnitude", "/left/right/attribute");
		check(OperatorKind.MULTIPLICATION, "/operator");
		check("gt0005", "/right/left/left/code");
		check("magnitude", "/right/left/left/attribute");
		check(OperatorKind.DIVISION, "/right/left/operator");
		check("70", "/right/left/right/value");
		check(OperatorKind.EXPONENT, "/right/operator");
		check("0.7", "right/right/value");
	}

    public void testNestedComplexArithmeticExpressions4() throws Exception {
        parseSingleBooleanExpression(E4);
        check("gt0003", "/left/left/code");
        check(OperatorKind.EQUALITY, "/left/operator");
    }



	public void testExpressionRoundTripStringToString() throws Exception {
		parseSingleExpression(E1);
		String actual = item.toString();
		assertEquals(E1, actual);
	}
	
	private static final String E1 =
			"((42.5*$gt0020|height|)/$gt0009|creatine|)*(($gt0007|weight|/70)^0.7)";

    private static final String E2 =
            "((1.23*(140-$gt0004|age|))*$gt0007|weight|)/$gt0009|creatine|";

	private static final String E3 =
			"((42.5 * $gt0007.magnitude) / $gt0009.magnitude) * (($gt0005.magnitude/70) ^ 0.7)";

    private static final String E4 =
            "($gt0003=='test1')||($gt0003=='test2')";
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