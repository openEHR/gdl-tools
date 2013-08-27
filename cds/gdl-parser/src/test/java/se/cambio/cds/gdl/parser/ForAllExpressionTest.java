package se.cambio.cds.gdl.parser;

import se.cambio.cds.gdl.model.expression.OperatorKind;

public class ForAllExpressionTest extends ExpressionTestBase {
	
	public void testForAllWithSingleCondition() throws Exception {
		parseSingleExpression("for_all($gt0009 !is_a ATC::B01AA03|Warfarin|)");
		check(OperatorKind.FOR_ALL, "/operator");
		check("gt0009", "/operand/left/code");		
		check(OperatorKind.IS_NOT_A, "/operand/operator");
		check("ATC", "/operand/right/codedText/definingCode/terminologyId/value");
		check("B01AA03", "/operand/right/codedText/definingCode/codeString");
		check("Warfarin", "/operand/right/codedText/value");		
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