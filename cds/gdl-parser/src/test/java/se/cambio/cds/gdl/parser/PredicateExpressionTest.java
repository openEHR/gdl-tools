package se.cambio.cds.gdl.parser;

import se.cambio.cds.gdl.model.expression.OperatorKind;

public class PredicateExpressionTest extends ExpressionTestBase {
	
	public void testPredicate() throws Exception {
		parseSingleExpression(EXP);		
		check("/data[at0001]/events[at0002]/data[at0003]/items[at0007]", "/left/path");
		check(OperatorKind.GREATER_THAN_OR_EQUAL, "/operator");
		check("20", "/right/value");
	}
	
	public void testRoundTrip() throws Exception {
		parseSingleExpression(EXP);		
		assertEquals(item.toString(), EXP);
	}
	
	private static final String EXP = 
			"/data[at0001]/events[at0002]/data[at0003]/items[at0007]>=20";
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