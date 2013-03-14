package se.cambio.cds.gdl.model.expression;

import junit.framework.TestCase;

public class UnaryExpressionTest extends TestCase {
	
	public void testToStringWithBooleanExpression() {
		BinaryExpression be = new BinaryExpression(new Variable("gt0001"), 
				new ConstantExpression("null"),	OperatorKind.INEQUAL);
		UnaryExpression ue = new UnaryExpression(be, OperatorKind.FOR_ALL);
		String expected = "for_all($gt0001!=null)";
		assertEquals(expected, ue.toString());
	}
	public void testToStringWithVariable() {
		UnaryExpression ue = new UnaryExpression(new Variable("gt0001"), 
				OperatorKind.FOR_ALL);
		String expected = "for_all($gt0001)";
		assertEquals(expected, ue.toString());
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