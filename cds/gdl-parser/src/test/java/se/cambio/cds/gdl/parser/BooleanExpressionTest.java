package se.cambio.cds.gdl.parser;

import org.openehr.rm.datatypes.quantity.DvQuantity;
import org.openehr.rm.datatypes.text.CodePhrase;

import se.cambio.cds.gdl.model.expression.*;

public class BooleanExpressionTest extends ExpressionTestBase {

	

	public void testSingleNotNull() throws Exception {
		parseSingleBooleanExpression("$gt0007 != null");
		Variable var = new Variable("gt0007");
		ConstantExpression c = new ConstantExpression("null");
		expected = new BinaryExpression(var, c, OperatorKind.INEQUAL);
		check();
	}
	
	public void testParseGreaterEqualsWithUnits() throws Exception {
		parseSingleBooleanExpression("$gt0003>=12.0,/min");
		Variable var = new Variable("gt0003");
		QuantityConstant c = new QuantityConstant(new DvQuantity("/min", 12.0, 1));
		expected = new BinaryExpression(var, c, OperatorKind.GREATER_THAN_OR_EQUAL);
		check();
	}
	
	public void testSingleVariableEquality() throws Exception {
		parseSingleExpression("$gt0001.value==$gt0003.value");
		check("gt0001", "/left/code");
		check("value", "/left/attribute");
		check("gt0003", "/right/code");
		check("value", "/right/attribute");		
	}
	
	public void testSingleVariableEqualityExtraParenthesis() throws Exception {
		parseSingleExpression("$gt0001.value==($gt0003.value)");
		check("gt0001", "/left/code");
		check("value", "/left/attribute");
		check("gt0003", "/right/code");
		check("value", "/right/attribute");		
	}

	public void testSingleNotNullWithLabel() throws Exception {
		parseSingleBooleanExpression("$gt0007 |weight| != null");
		Variable var = new Variable("gt0007", "weight", null);
		ConstantExpression c = new ConstantExpression("null");
		expected = new BinaryExpression(var, c, OperatorKind.INEQUAL);
		check();
	}
	
	public void testSingleGreaterThanInt() throws Exception {
		parseSingleBooleanExpression("$gt0004 > 20");
		Variable var = new Variable("gt0004");
		ConstantExpression c = new ConstantExpression("20");
		expected = new BinaryExpression(var, c, OperatorKind.GREATER_THAN);
		check();
	}
	
	public void testSingleISA() throws Exception {
		parseSingleBooleanExpression("$gt0003 is_a ICD10::I50|Heart failure|");
		Variable var = new Variable("gt0003");
		ConstantExpression c = new CodedTextConstant("Heart failure", 
				new CodePhrase("ICD10", "I50"));
		expected = new BinaryExpression(var, c, OperatorKind.IS_A);
		check();
	}

	public void testSingleGreaterThanIntWithLabel() throws Exception {
		parseSingleBooleanExpression("$gt0004 |age| > 20");
		Variable var = new Variable("gt0004", "age");
		ConstantExpression c = new ConstantExpression("20");
		expected = new BinaryExpression(var, c, OperatorKind.GREATER_THAN);
		check();
	}
	
	public void testSingleLessThanInt() throws Exception {
		parseSingleBooleanExpression("$gt0004 < 20");
		Variable var = new Variable("gt0004");
		ConstantExpression c = new ConstantExpression("20");
		expected = new BinaryExpression(var, c, OperatorKind.LESS_THAN);
		check();
	}
	
	public void testSingleLessThanEqualInt() throws Exception {
		parseSingleBooleanExpression("$gt0004 <= 20");
		Variable var = new Variable("gt0004");
		ConstantExpression c = new ConstantExpression("20");
		expected = new BinaryExpression(var, c, OperatorKind.LESS_THAN_OR_EQUAL);
		check();
	}
	
	public void testSingleEqualInt() throws Exception {
		parseSingleBooleanExpression("$gt0004 == 20");
		Variable var = new Variable("gt0004");
		ConstantExpression c = new ConstantExpression("20");
		expected = new BinaryExpression(var, c, OperatorKind.EQUALITY);
		check();
	}
	
	public void testSingleEqualIntWithParenthesis() throws Exception {
		parseSingleBooleanExpression("$gt0004 == (20)");
		Variable var = new Variable("gt0004");
		ConstantExpression c = new ConstantExpression("20");
		expected = new BinaryExpression(var, c, OperatorKind.EQUALITY);
		check();
	}

	public void testSingleLessThanIntWithLabel() throws Exception {
		parseSingleBooleanExpression("$gt0004 |age| < 20");
		Variable var = new Variable("gt0004", "age");
		ConstantExpression c = new ConstantExpression("20");
		expected = new BinaryExpression(var, c, OperatorKind.LESS_THAN);
		check();
	}
	
	public void testSingleGreaterThanIntEqual() throws Exception {
		parseSingleBooleanExpression("$gt0004 >= 20");
		Variable var = new Variable("gt0004");
		ConstantExpression c = new ConstantExpression("20");
		expected = new BinaryExpression(var, c, OperatorKind.GREATER_THAN_OR_EQUAL);
		check();
	}

	public void testSingleGreaterThanIntEqualWithLabel() throws Exception {
		parseSingleBooleanExpression("$gt0004 |age| >= 20");
		Variable var = new Variable("gt0004", "age");
		ConstantExpression c = new ConstantExpression("20");
		expected = new BinaryExpression(var, c, OperatorKind.GREATER_THAN_OR_EQUAL);
		check();
	}
	
	public void testSingleGreaterThanReal() throws Exception {
		parseSingleBooleanExpression("$gt0011 > 25.0");
		Variable var = new Variable("gt0011");
		ConstantExpression c = new ConstantExpression("25.0");
		expected = new BinaryExpression(var, c, OperatorKind.GREATER_THAN);
		check();
	}

	public void testSingleGreaterThanRealWithLabel() throws Exception {
		parseSingleBooleanExpression("$gt0011 |GFR| > 25.0");
		Variable var = new Variable("gt0011", "GFR");
		ConstantExpression c = new ConstantExpression("25.0");
		expected = new BinaryExpression(var, c, OperatorKind.GREATER_THAN);
		check();
	}	
	
	public void testSingleAnd() throws Exception {
		parseSingleBooleanExpression("$gt0011 and $gt0012");
		Variable var1 = new Variable("gt0011");
		Variable var2 = new Variable("gt0012");
		expected = new BinaryExpression(var1, var2, OperatorKind.AND);
		check();
	}
	
	public void testEqualityWithMultipleVariablesAndAttributes() throws Exception {
		parseSingleExpression("$gt0001.value==($currentDateTime.year+$gt0003.value)");	
		check("gt0001", "/left/code");
		check("value", "/left/attribute");
		check(OperatorKind.EQUALITY, "/operator");
		check("currentDateTime", "/right/left/code");
		check("year", "/right/left/attribute");
		check(OperatorKind.ADDITION, "/right/operator");
		check("gt0003", "/right/right/code");
		check("value", "/right/right/attribute");
	}
	
	public void testISAWithDoubleSemiColonsInLabel() throws Exception {
		parseSingleBooleanExpression("$gt0003 is_a ICD10::I12|ICD10::I12|");
	}
	
	public void testISNoTA() throws Exception {
		parseSingleBooleanExpression("$gt0003 !is_a ICD10::I12");
		check("gt0003", "/left/code");
		check(OperatorKind.IS_NOT_A, "/operator");
	}
	
	public void testISNoTAWithLabel() throws Exception {
		parseSingleBooleanExpression("$gt0009 !is_a ATC::B01AA03|Warfarin|");
		check("gt0009", "/left/code");
		check(OperatorKind.IS_NOT_A, "/operator");
		check("ATC", "/right/codedText/definingCode/terminologyId/value");
		check("B01AA03", "/right/codedText/definingCode/codeString");
		check("Warfarin", "/right/codedText/value");
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