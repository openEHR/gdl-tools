package se.cambio.cds.gdl.parser;

import junit.framework.TestCase;
import org.apache.commons.jxpath.JXPathContext;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.expression.OperatorKind;

import java.io.InputStream;

public class GDLParsingTest extends TestCase {
	
	
	public void setUp() {
		parser = new GDLParser();
		guide = null;
	}
	
	public void testParseSimpleGDL() throws Exception {
		parse("test001.dadl");
		check("/data[at0002]/events[at0003]/data[at0001]/items[at0004]", 
				"definition/archetypeBindings/gt0004/elements/gt0007/path");
	}

	public void test_can_parse_element_with_type() throws Exception {
		parse("test_parse_element_with_type.gdl");
		check("DV_QUANTITY", "definition/archetypeBindings/gt0002/elements/gt0003/type");
	}
	
	public void testSetId() throws Exception {
		parse("test001.dadl");
		check("gt0007", "definition/archetypeBindings/gt0004/elements/gt0007/id");
		check("gt0030", "definition/rules/gt0030/id");
	}
	
	public void testPathPredicatesParsing() throws Exception {
		parse("test001.dadl");
		
		// first predicate
		check("/data[at0001]/events[at0002]/data[at0003]/items[at0007]", 
				"definition/archetypeBindings/gt0004/predicateStatements[1]/left/path");
		check(OperatorKind.GREATER_THAN_OR_EQUAL, 
				"definition/archetypeBindings/gt0004/predicateStatements[1]/operator");
		check("20", 
				"definition/archetypeBindings/gt0004/predicateStatements[1]/right/value");
		
		// second predicate
		check("/data[at0002]/events[at0003]/data[at0001]/items[at0004]", 
				"definition/archetypeBindings/gt0004/predicateStatements[2]/left/path");
		check("10", 
				"definition/archetypeBindings/gt0004/predicateStatements[2]/right/value");
		
	}
	
	public void testGetResourceDescriptionItem() throws Exception {
		parse("test001.dadl");
		check("Dient", "description/details/de/purpose");		
	}
	
	public void testGetTranslations() throws Exception {
		parse("test001.dadl");
		check("Shahla Foozonkhah", "language/translations/fa/author/name");		
	}

	public void testSpecialCharactersOnExpressions() throws Exception {
		parse("test_with_special_character.gdl");
	}

	
	private void parse(String input) throws Exception {
		guide = parser.parse(load(input));
		assertNotNull(guide);
	}
	
	private InputStream load(String name) throws Exception {
		return this.getClass().getClassLoader()
				.getResourceAsStream(name);
	}
	
	private void check(Object expected, String path) throws Exception {
		JXPathContext context = JXPathContext.newContext(guide);
		assertEquals(expected, context.getValue(path));
	}
	
	private GDLParser parser;
	private Guide guide;
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