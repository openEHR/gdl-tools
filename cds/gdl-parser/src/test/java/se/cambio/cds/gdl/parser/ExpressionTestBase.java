package se.cambio.cds.gdl.parser;

import java.io.Reader;
import java.io.StringReader;
import java.util.List;

import org.apache.commons.jxpath.JXPathContext;

import se.cambio.cds.gdl.model.expression.BinaryExpression;
import se.cambio.cds.gdl.model.expression.ExpressionItem;
import junit.framework.TestCase;

public class ExpressionTestBase extends TestCase {
	public void setUp() {
		item = null;
		items = null;
		be = null;
		expected = null;
	}

	void parseSingleExpression(String value) throws Exception {
		parseArithmeticExpressions(value);
		assertNotNull(items);
		assertEquals(1, items.size());
		item = items.get(0);
		if(item instanceof BinaryExpression) {
			be = (BinaryExpression) item;
		}
	}

	void parseSingleBooleanExpression(String value) throws Exception {
		parseBooleanExpressions(value);
		assertNotNull(items);
		assertEquals(1, items.size());
		item = items.get(0);
		assertTrue("unexpected type: " + item.getClass(),
				item instanceof BinaryExpression);
		be = (BinaryExpression) item;
	}

	void check() throws Exception {
		assertEquals(expected, be);
	}
	
	void parseArithmeticExpressions(String value) throws Exception {
		items = Expressions.parseArithmeticExpressions(value);
	}
	
	void parseBooleanExpressions(String value) throws Exception {
		items = Expressions.parseBooleanExpressions(value);
	}
	
	Reader convert(String value) throws Exception {
		return new StringReader(value);
	}

	void check(Object expected, String path) throws Exception {
		JXPathContext context = JXPathContext.newContext(item);
		Object obj = context.getValue(path);
		assertEquals(expected, obj);
	}

	List<ExpressionItem> items;
	ExpressionItem item;
	BinaryExpression be;
	BinaryExpression expected;
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