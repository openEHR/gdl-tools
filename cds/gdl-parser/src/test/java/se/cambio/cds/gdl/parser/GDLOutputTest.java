package se.cambio.cds.gdl.parser;

import junit.framework.TestCase;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.Rule;
import se.cambio.cds.gdl.model.TranslationDetails;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class GDLOutputTest extends TestCase {
	
	public void setUp() {
		parser = new GDLParser();	
		serializer = new DADLSerializer();
	}
	
	public void tearDown() {
		guide = null;
	}
	
	public void testParseSimpleGDL() throws Exception {
		parse("test001.dadl");
		List<String> list = serializer.toDADL(guide);
		write(list, "output.dadl");
	}
	
	public void testOutputEmptyMap() throws Exception {
		parse("test001.dadl");
		
		guide.getLanguage().setTranslations(new HashMap<String,TranslationDetails>());
		List<String> list = serializer.toDADL(guide);		
		write(list, "output2.dadl");
		
		Guide actual = parse(list);
		guide.getLanguage().setTranslations(null);
		assertEquals(guide, actual);
	}
	
	public void testRoundTrip() throws Exception {
		parse("test001.dadl");
		List<String> list = serializer.toDADL(guide);
		
		write(list, "after_parse.dadl");
		
		StringBuffer buf = new StringBuffer();
		for(String s : list) {
			buf.append(s);
		}
		Reader reader = convert(buf.toString());
		Guide after = parser.parse(reader);
		
		list = serializer.toDADL(after);
		write(list, "after_round_trip.dadl");
		
		assertEquals(guide, after);
	}
	
	public void testRuleHasRightOrder() throws Exception {
		Rule rule = new Rule();
		rule.setId("gt0001");
		List<String> list = new ArrayList<String>();
		list.add("when1");
		rule.setWhen(list);
		list = new ArrayList<String>();
		list.add("then1");
		rule.setThen(list);
		
		list = serializer.toDADL(rule);
		assertEquals(5, list.size());
		assertTrue(list.get(1).trim().startsWith("when"));
		assertTrue(list.get(2).trim().startsWith("then"));
	}
	
	public void testRoundTripDate() throws Exception {
	    roundTrip("test_date.gdl");
	}

	public void testRoundTripText() throws Exception {
	    roundTrip("test_text.gdl");
	}
	
	public void testRoundTripNegativeNumber() throws Exception {
	    roundTrip("test_negative_number.gdl");
	}

	public static void roundTrip(String guideFilename) throws Exception {
	    InputStream is = load(guideFilename);
	    InputStreamReader in = new InputStreamReader(is, "UTF-8");
	    String str1 = IOUtils.toString(in).replaceAll("\\r\\n", "\n");
	    GDLParser parser = new GDLParser();
	    Guide guide = parser.parse(new ByteArrayInputStream(str1.getBytes()));
	    StringBuffer sb = new StringBuffer();
	    DADLSerializer serializer = new DADLSerializer();
	    for (String line : serializer.toDADL(guide)) {
		sb.append(line+"\n");
	    }
	    String str2 = sb.toString();
	    assertEquals(str1, str2);
	}
	
	private Guide parse(List<String> lines) throws Exception {
		StringBuffer buf = new StringBuffer();
		for(String s : lines) {
			buf.append(s);
		}
		return parser.parse(convert(buf.toString()));
	}
	
	private static Reader convert(String value) throws Exception {
		return new StringReader(value);
	}
	
	private void parse(String input) throws Exception {
		guide = parser.parse(load(input));
	}
	
	private static InputStream load(String name) throws Exception {
	    return GDLOutputTest.class.getClassLoader().getResourceAsStream(name);
	}
	
	private void write(List<String> lines, String file) throws Exception {
		FileUtils.writeLines(new File(file), lines);
	}
	
	private GDLParser parser;
	private Guide guide;
	private DADLSerializer serializer;
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