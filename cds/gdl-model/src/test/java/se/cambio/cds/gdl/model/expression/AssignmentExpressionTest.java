package se.cambio.cds.gdl.model.expression;

import org.openehr.rm.datatypes.text.CodePhrase;

import junit.framework.TestCase;

public class AssignmentExpressionTest extends TestCase {
	
	public void testToStringWithIntegerConstant() {
		AssignmentExpression ae = new AssignmentExpression(
				new Variable("gt0001"), new ConstantExpression("10"));
		assertEquals("$gt0001=10", ae.toString());
	}
	
	public void testToStringWithCodedTextConstant() {
		AssignmentExpression ae = new AssignmentExpression(
				new Variable("gt0001"), new CodedTextConstant(
						"Warfarin", new CodePhrase("ATC", "B01AA03")));
		assertEquals("$gt0001=ATC::B01AA03|Warfarin|", ae.toString());
	}
	
	public void testToStringWithStringConstant() {
		AssignmentExpression ae = new AssignmentExpression(
				new Variable("gt0001"), new StringConstant("sitting"));
		assertEquals("$gt0001='sitting'", ae.toString());
	}
	
	public void testToStringWithBooleanExpression() {
		Variable var1 = new Variable("gt0010");
		Variable var2 = new Variable("gt0012");
		BinaryExpression be = new BinaryExpression(var1, var2, OperatorKind.ADDITION);
		AssignmentExpression ae = new AssignmentExpression(
				new Variable("gt0001"), be);
		assertEquals("$gt0001=($gt0010+$gt0012)", ae.toString());
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