package se.cambio.cds.gdl.parser;

import se.cambio.cds.gdl.model.expression.*;

import java.util.ArrayList;
import java.util.List;

public class MathematicalExpressionTest extends ExpressionTestBase {

	public void testLog() throws Exception {
		parseSingleExpression("log(1)");
		List<ExpressionItem> functionItems = new ArrayList<>();
		functionItems.add(new ConstantExpression("1"));
		expected = new FunctionalExpression(new Function("log"), functionItems);
		check();
	}

	public void testLogWithSubExpressions() throws Exception {
		parseSingleExpression("log((1+2))");
		List<ExpressionItem> functionItems = new ArrayList<>();
		functionItems.add(new BinaryExpression(
				new ConstantExpression("1"),
				new ConstantExpression("2"),
				OperatorKind.ADDITION));
		expected = new FunctionalExpression(new Function("log"), functionItems);
		check();
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