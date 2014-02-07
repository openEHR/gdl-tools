package se.cambio.cds.gdl.model.expression;

import se.cambio.cds.gdl.model.expression.BinaryExpression;
import se.cambio.cds.gdl.model.expression.ConstantExpression;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.gdl.model.expression.Variable;
import junit.framework.TestCase;

public class ArithmeticExpressionTest extends TestCase {	
	
	public void testPrintNestedExpressions() throws Exception {
		BinaryExpression exp1 = new BinaryExpression(new Variable("gt0007"),
				new ConstantExpression("70"), OperatorKind.DIVISION);
		BinaryExpression exp2 = new BinaryExpression(exp1, 
				new ConstantExpression("0.7"), OperatorKind.EXPONENT);
		BinaryExpression exp3 = new BinaryExpression(new ConstantExpression("42.5"),
				new Variable("gt0020"), OperatorKind.MULTIPLICATION);
		BinaryExpression exp4 = new BinaryExpression(exp3, new Variable("gt0009"),
				OperatorKind.DIVISION);
		BinaryExpression expression = new BinaryExpression(exp4, exp2, OperatorKind.DIVISION);
		
		String expected = "((42.5*$gt0020)/$gt0009)/(($gt0007/70)^0.7)";
		String actual = expression.toString();
		assertEquals(expected, actual);
	}
	
	public void testPrintNestedExpressionsWithVariableName() throws Exception {
		BinaryExpression exp1 = new BinaryExpression(new Variable("gt0007", "weight"),
				new ConstantExpression("70"), OperatorKind.DIVISION);
		BinaryExpression exp2 = new BinaryExpression(exp1, 
				new ConstantExpression("0.7"), OperatorKind.EXPONENT);
		BinaryExpression exp3 = new BinaryExpression(new ConstantExpression("42.5"),
				new Variable("gt0020", "height"), OperatorKind.MULTIPLICATION);
		BinaryExpression exp4 = new BinaryExpression(exp3, new Variable("gt0009", "creatine"),
				OperatorKind.DIVISION);
		BinaryExpression expression = new BinaryExpression(exp4, exp2, OperatorKind.DIVISION);
		
		String expected = "((42.5*$gt0020|height|)/$gt0009|creatine|)/(($gt0007|weight|/70)^0.7)";
		String actual = expression.toString();
		assertEquals(expected, actual);
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