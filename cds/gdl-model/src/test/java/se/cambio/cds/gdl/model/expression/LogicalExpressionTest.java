package se.cambio.cds.gdl.model.expression;

import junit.framework.TestCase;
import org.openehr.rm.datatypes.text.CodePhrase;

public class LogicalExpressionTest extends TestCase {

	public void testCreateSimpleGreaterThanExpression() {
		BinaryExpression be = BinaryExpression
				.create(Variable.createByCode("gt0001"),
						ConstantExpression.create("100"),
						OperatorKind.GREATER_THAN);

		String actual = be.toString();
		String expected = "$gt0001>100";
		assertEquals(expected, actual);
	}
	
	public void testCreateISAExpression() {
		BinaryExpression be = BinaryExpression
				.create(Variable.createByCode("gt0001"),
						new CodePhraseConstant(new CodePhrase("ICD10", "1000")),
						OperatorKind.IS_A);

		String actual = be.toString();
		String expected = "$gt0001 is_a ICD10::1000";
		assertEquals(expected, actual);
	}

	public void testCreateNestedAndLogicalExpression() {
		BinaryExpression be1 = BinaryExpression.create(
				Variable.createByCode("gt0001"),
				ConstantExpression.create("100"), OperatorKind.LESS_THAN);

		BinaryExpression be2 = BinaryExpression.create(
				Variable.createByCode("gt0001"),
				ConstantExpression.create("10"),
				OperatorKind.GREATER_THAN_OR_EQUAL);

		BinaryExpression be3 = BinaryExpression.create(be1, be2,
				OperatorKind.AND);

		String actual = be3.toString();
		String expected = "($gt0001<100)&&($gt0001>=10)";
		assertEquals(expected, actual);
	}

	public void testCreateDoubleNestedORLogicalExpression() {
		BinaryExpression be1 = BinaryExpression.create(
				Variable.createByCode("gt0001"),
				ConstantExpression.create("100"), OperatorKind.LESS_THAN);

		BinaryExpression be2 = BinaryExpression.create(
				Variable.createByCode("gt0001"),
				ConstantExpression.create("10"),
				OperatorKind.GREATER_THAN_OR_EQUAL);

		BinaryExpression be3 = BinaryExpression.create(be1, be2,
				OperatorKind.AND);

		BinaryExpression be4 = BinaryExpression.create(
				Variable.createByCode("gt0002"),
				ConstantExpression.create("30"),
				OperatorKind.LESS_THAN_OR_EQUAL);

		BinaryExpression be5 = BinaryExpression.create(be4, be3,
				OperatorKind.OR);

		String actual = be5.toString();
		String expected = "($gt0002<=30)||(($gt0001<100)&&($gt0001>=10))";
		assertEquals(expected, actual);
	}
	
	public void testStringEqualityExpression() {
		BinaryExpression be = BinaryExpression
				.create(Variable.createByCode("gt0001"),
						new StringConstant("string value"),
						OperatorKind.EQUALITY);

		String actual = be.toString();
		String expected = "$gt0001=='string value'";
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