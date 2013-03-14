package se.cambio.cds.gdl.parser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import junit.framework.TestCase;

import org.apache.commons.jxpath.JXPathContext;

import se.cambio.cds.gdl.model.Term;

public class CreateModelClassTest extends TestCase {

	public void setUp() {
		binding = new GDLBinding();
		valueMap = new HashMap<String,Object>();
	}
	
	public void tearDown() {
		binding = null;
		ret = null;
		valueMap = null;
	}
	
	public void testCreateTerm() throws Exception {
		valueMap.put("id", "gt0001");
		valueMap.put("text", "text value");
		valueMap.put("description", "desc value");
		
		ret = binding.createModelClass("TERM", valueMap);
		assertTrue(ret instanceof Term);
		Term t = (Term) ret;
		assertEquals("gt0001", t.getId());
		assertEquals("text value", t.getText());
		assertEquals("desc value", t.getDescription());
	}
	
	public void testCreateRule() throws Exception {
		valueMap.put("id", "gt0010");
		List<String> whenList = new ArrayList<String>();
		whenList.add("gt0021 >= 10");
		valueMap.put("when",whenList);
		List<String> thenList = new ArrayList<String>();
		whenList.add("gt0022 == 11");
		valueMap.put("then",thenList);		
		ret = binding.createModelClass("RULE", valueMap);
	}
	
	private void check(Object expected, String path) throws Exception {
		JXPathContext context = JXPathContext.newContext(ret);
		assertEquals(expected, context.getValue(path));
	}
	
	private GDLBinding binding;
	private Object ret;
	private Map<String,Object> valueMap;
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