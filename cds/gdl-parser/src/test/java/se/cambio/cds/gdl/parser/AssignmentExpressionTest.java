package se.cambio.cds.gdl.parser;

import se.cambio.cds.gdl.model.expression.OperatorKind;

public class AssignmentExpressionTest extends ExpressionTestBase {
	
	public void testIntegerAssignmentWithVariableAttribute() throws Exception {
		parseSingleExpression("$gt0011.precision = 2");
		check("gt0011", "/variable/code");
		check("precision", "/variable/attribute");
		check("2", "/assignment/value");
	}
	
	public void testIntegerAssignmentWithVariable() throws Exception {
		parseSingleExpression("$gt0011 = 2");
		check("gt0011", "/variable/code");
		check("2", "/assignment/value");
	}
	
	public void testAssignmentWithOrdinal() throws Exception {
		parseSingleExpression("$gt0011=1|local::at0037|Between 65-74|");
		check(1, "/assignment/ordinal/value");
		check("local", "/assignment/ordinal/symbol/definingCode/terminologyId/value");
		check("at0037", "/assignment/ordinal/symbol/definingCode/codeString");
		check("Between 65-74", "/assignment/ordinal/symbol/value");
		check("gt0011", "/variable/code");
	}
	
	public void testAssignmentWithOrdinalSpecialChars() throws Exception {
		parseSingleExpression("$gt0004=3|local::at0011|<=8 or >=25|");
		check(3, "/assignment/ordinal/value");
		check("local", "/assignment/ordinal/symbol/definingCode/terminologyId/value");
		check("at0011", "/assignment/ordinal/symbol/definingCode/codeString");
		check("<=8 or >=25", "/assignment/ordinal/symbol/value");
		check("gt0004", "/variable/code");
	}
	
	public void testAssignmentWithOrdinalInsideParenthesis() throws Exception {
		parseSingleExpression("$gt0011=(1|local::at0028|Present|)");
		check(1, "/assignment/ordinal/value");
		check("local", "/assignment/ordinal/symbol/definingCode/terminologyId/value");
		check("at0028", "/assignment/ordinal/symbol/definingCode/codeString");
		check("Present", "/assignment/ordinal/symbol/value");
		check("gt0011", "/variable/code");
	}
		
	public void testComplexArithmeticAssignmentWithLabels() throws Exception {
		parseSingleExpression(COMPLEX);		
		check("gt0011", "/variable/code");
		check("GFR", "/variable/name");
		check("creatine", "/assignment/right/name");
		check(OperatorKind.DIVISION, "/assignment/operator");
	}
	
	public void testAssignmentWithAttributesOnBothSides() throws Exception {
		parseSingleExpression("$gt0011.attr1=$gt0012.attr2");		
		check("gt0011", "/variable/code");
		check("attr1", "/variable/attribute");
		check("gt0012", "/assignment/code");
		check("attr2", "/assignment/attribute");
	}
	
	public void testAssignmentWithAttributesOnLeftSide() throws Exception {
		parseSingleExpression("$gt0011.attr1=$gt0012");		
		check("gt0011", "/variable/code");
		check("attr1", "/variable/attribute");
		check("gt0012", "/assignment/code");
	}
	
	public void testAssignmentWithAttributesOnRightSide() throws Exception {
		parseSingleExpression("$gt0011=$gt0012.attr2");		
		check("gt0011", "/variable/code");
		check("gt0012", "/assignment/code");
		check("attr2", "/assignment/attribute");
	}
	
	public void testAssignmentWithoutAttribute() throws Exception {
		parseSingleExpression("$gt0011=$gt0012");		
		check("gt0011", "/variable/code");
		check("gt0012", "/assignment/code");
	}	
	
	public void testAssignmentAttributeWithInteger() throws Exception {
		parseSingleExpression("$gt0011.precision=2");		
		check("gt0011", "/variable/code");
		check("precision", "/variable/attribute");
		check("2", "/assignment/value");
	}	
	
	public void testAssignmentWithMultipleVariablesAndAttributes() throws Exception {
		parseSingleExpression("$gt0001.value=($currentDateTime.year+$gt0003.value)");	
		check("gt0001", "/variable/code");
		check("value", "/variable/attribute");
		check("currentDateTime", "/assignment/left/code");
		check("year", "/assignment/left/attribute");
		check("gt0003", "/assignment/right/code");
		check("value", "/assignment/right/attribute");
	}	
	
	public void testComplexArithmeticAssignmentRoundTrip() throws Exception {
		parseSingleExpression(COMPLEX);
		String actual = item.toString();
		assertEquals(actual, COMPLEX);		
	}
	
	private static final String COMPLEX = 
		"$gt0011|GFR|=(((1.23*(140-$gt0004|age|))*$gt0007|weight|)/$gt0009|creatine|)";
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