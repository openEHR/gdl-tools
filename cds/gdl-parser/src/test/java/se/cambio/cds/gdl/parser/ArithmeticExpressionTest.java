package se.cambio.cds.gdl.parser;

import org.openehr.rm.datatypes.quantity.DvQuantity;

import se.cambio.cds.gdl.model.expression.*;

public class ArithmeticExpressionTest extends ExpressionTestBase {
	
	public void testAdditionTwoVars() throws Exception {
		parseSingleExpression("$gt0010 + $gt0012");
		Variable var1 = new Variable("gt0010");
		Variable var2 = new Variable("gt0012");
		expected = new BinaryExpression(var1, var2, OperatorKind.ADDITION);
		check();
	}
	
	public void testSubstrationTwoVars() throws Exception {
		parseSingleExpression("$gt0010 - $gt0012");
		Variable var1 = new Variable("gt0010");
		Variable var2 = new Variable("gt0012");
		expected = new BinaryExpression(var1, var2, OperatorKind.SUBSTRATION);
		check();
	}
	
	public void testMultiplicationTwoVars() throws Exception {
		parseSingleExpression("$gt0010 * $gt0012");
		Variable var1 = new Variable("gt0010");
		Variable var2 = new Variable("gt0012");
		expected = new BinaryExpression(var1, var2, OperatorKind.MULTIPLICATION);
		check();
	}
	
	public void testDivisionTwoVars() throws Exception {
		parseSingleExpression("$gt0010 / $gt0012");
		Variable var1 = new Variable("gt0010");
		Variable var2 = new Variable("gt0012");
		expected = new BinaryExpression(var1, var2, OperatorKind.DIVISION);
		check();
	}
	
	public void testExponentTwoVars() throws Exception {
		parseSingleExpression("$gt0010 ^ $gt0012");
		Variable var1 = new Variable("gt0010");
		Variable var2 = new Variable("gt0012");
		expected = new BinaryExpression(var1, var2, OperatorKind.EXPONENT);
		check();
	}
	
	public void testAdditionConstVar() throws Exception {
		parseSingleExpression("10 + $gt0012");
		ConstantExpression var1 = new ConstantExpression("10");
		Variable var2 = new Variable("gt0012");
		expected = new BinaryExpression(var1, var2, OperatorKind.ADDITION);
		check();
	}
	
	public void testSubstrationConstVar() throws Exception {
		parseSingleExpression("10 - $gt0012");
		ConstantExpression var1 = new ConstantExpression("10");
		Variable var2 = new Variable("gt0012");
		expected = new BinaryExpression(var1, var2, OperatorKind.SUBSTRATION);
		check();
	}
	
	public void testMultiplicationConstVar() throws Exception {
		parseSingleExpression("10 * $gt0012");
		ConstantExpression var1 = new ConstantExpression("10");
		Variable var2 = new Variable("gt0012");
		expected = new BinaryExpression(var1, var2, OperatorKind.MULTIPLICATION);
		check();
	}
	
	public void testDivisionConstVar() throws Exception {
		parseSingleExpression("10 / $gt0012");
		ConstantExpression var1 = new ConstantExpression("10");
		Variable var2 = new Variable("gt0012");
		expected = new BinaryExpression(var1, var2, OperatorKind.DIVISION);
		check();
	}
	
	public void testExponentConstVar() throws Exception {
		parseSingleExpression("10 ^ $gt0012");
		ConstantExpression var1 = new ConstantExpression("10");
		Variable var2 = new Variable("gt0012");
		expected = new BinaryExpression(var1, var2, OperatorKind.EXPONENT);
		check();
	}
	
	public void testExponentVarInteger() throws Exception {
		parseSingleExpression("$gt0012 ^ 10");
		ConstantExpression var1 = new ConstantExpression("10");
		Variable var2 = new Variable("gt0012");
		expected = new BinaryExpression(var2, var1, OperatorKind.EXPONENT);
		check();
	}
	
	public void testExponentVarDouble() throws Exception {
		parseSingleExpression("$gt0012 ^ 0.7");
		ConstantExpression var1 = new ConstantExpression("0.7");
		Variable var2 = new Variable("gt0012");
		expected = new BinaryExpression(var2, var1, OperatorKind.EXPONENT);
		check();
	}
	
	public void testAdditionVarConst() throws Exception {
		parseSingleExpression("$gt0010 + 12");
		Variable var1 = new Variable("gt0010");
		ConstantExpression var2 = new ConstantExpression("12");
		expected = new BinaryExpression(var1, var2, OperatorKind.ADDITION);
		check();
	}
	
	public void testSubstrationVarConst() throws Exception {
		parseSingleExpression("$gt0010 - 12");
		Variable var1 = new Variable("gt0010");
		ConstantExpression var2 = new ConstantExpression("12");
		expected = new BinaryExpression(var1, var2, OperatorKind.SUBSTRATION);
		check();
	}
	
	public void testMultiplicationVarConst() throws Exception {
		parseSingleExpression("$gt0010 * 12");
		Variable var1 = new Variable("gt0010");
		ConstantExpression var2 = new ConstantExpression("12");
		expected = new BinaryExpression(var1, var2, OperatorKind.MULTIPLICATION);
		check();
	}
	
	public void testDivisionVarConst() throws Exception {
		parseSingleExpression("$gt0010 / 12");
		Variable var1 = new Variable("gt0010");
		ConstantExpression var2 = new ConstantExpression("12");
		expected = new BinaryExpression(var1, var2, OperatorKind.DIVISION);
		check();
	}
	
	public void testExponentVarConst() throws Exception {
		parseSingleExpression("$gt0010 ^ 12");
		Variable var1 = new Variable("gt0010");
		ConstantExpression var2 = new ConstantExpression("12");
		expected = new BinaryExpression(var1, var2, OperatorKind.EXPONENT);
		check();
	}
	
	public void testAdditionTwoVarsLabel() throws Exception {
		parseSingleExpression("$gt0010|height| + $gt0012|weight|");
		Variable var1 = new Variable("gt0010", "height");
		Variable var2 = new Variable("gt0012", "weight");
		expected = new BinaryExpression(var1, var2, OperatorKind.ADDITION);
		check();
	}
	
	public void testSubstrationTwoVarsLabel() throws Exception {
		parseSingleExpression("$gt0010|height| - $gt0012|weight|");
		Variable var1 = new Variable("gt0010", "height");
		Variable var2 = new Variable("gt0012", "weight");
		expected = new BinaryExpression(var1, var2, OperatorKind.SUBSTRATION);
		check();
	}
	
	public void testMultiplicationTwoVarsLabel() throws Exception {
		parseSingleExpression("$gt0010|height| * $gt0012|weight|");
		Variable var1 = new Variable("gt0010", "height");
		Variable var2 = new Variable("gt0012", "weight");
		expected = new BinaryExpression(var1, var2, OperatorKind.MULTIPLICATION);
		check();
	}
	
	public void testDivisionTwoVarsLabel() throws Exception {
		parseSingleExpression("$gt0010|height| / $gt0012|weight|");
		Variable var1 = new Variable("gt0010", "height");
		Variable var2 = new Variable("gt0012", "weight");
		expected = new BinaryExpression(var1, var2, OperatorKind.DIVISION);
		check();
	}
	
	public void testExponentTwoVarsLabel() throws Exception {
		parseSingleExpression("$gt0010|height| ^ $gt0012|weight|");
		Variable var1 = new Variable("gt0010", "height");
		Variable var2 = new Variable("gt0012", "weight");
		expected = new BinaryExpression(var1, var2, OperatorKind.EXPONENT);
		check();
	}
	
	public void testParseSpecialUnits() throws Exception {
		parseSingleExpression("$gt0010|Creatinine| > 62,µmol/l");
		Variable var1 = new Variable("gt0010", "Creatinine");
		QuantityConstant var2 = new QuantityConstant(new DvQuantity("µmol/l", 62.0, 0));
		expected = new BinaryExpression(var1, var2, OperatorKind.GREATER_THAN);
		check();
	}
	
	public void testParseNegativeNumber() throws Exception {
		parseSingleExpression("$gt0010|Creatinine|^(-0.329)");
		Variable var1 = new Variable("gt0010", "Creatinine");
		ConstantExpression var2 = new ConstantExpression("(-0.329)");
		expected = new BinaryExpression(var1, var2, OperatorKind.EXPONENT);
		check();
	}
	
	public void testComplexOneWithMinusNumber() throws Exception {
		parseSingleExpression(
				"$gt0007.magnitude=((144*(((($gt0003.magnitude/0.7)*11312)/10000000)^(-0.329)))*(0.993^$gt0008.magnitude))");
	}
	
	public void testParseAssignementWithDecimal() throws Exception {
		parseSingleExpression(
				"$gt0013.magnitude=($gt0005*0.007184)");
	}
	
	public void testParseAssignementWithMinusDecimal() throws Exception {
		parseSingleExpression(
				"$gt0013.magnitude=($gt0005*(-0.007184))");
	}
	
	public void testParseEvaluationWithSubstraction() throws Exception {
		parseSingleExpression(
				"$gt0002.value>=($currentDateTime.value-4.5,h)");
	}
	
	public void testParseAssignementWithMinusInteger() throws Exception {
		parseSingleExpression(
				"$gt0013.magnitude=($gt0005*(-3))");
	}
	
	public void testParseSubstractionWithMinusInteger() throws Exception {
		parseSingleExpression(
				"$gt0013.magnitude=($gt0005-(-3))");
	}
	
	public void testParseAssignmentWithMinusIntegerWithUnits() throws Exception {
		parseSingleExpression("$gt0013.magnitude=(-3),d");
	}
	
	public void testParseSubstractionWithMinusIntegerWithUnits() throws Exception {
		parseSingleExpression(
				"$gt0013.magnitude=($gt0005*(-3),d)");
	}
	
	public void testParseSubstractionWithMinusDecimalWithUnits() throws Exception {
		parseSingleExpression(
				"$gt0013.magnitude=($gt0005*(-3.0),d)");
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