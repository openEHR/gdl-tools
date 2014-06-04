package se.cambio.cds.gdl.parser;

public class InstanceCreationExpressionTest extends ExpressionTestBase {

    public void testInstanceCreationWithMultipleSets() throws Exception {
        parseSingleExpression("$gt0003.create($gt0009=20,kg;$gt0010=1|local::at0028|Present|)");
        check("gt0003", "/variable/code");
        check("create", "/variable/attribute");
        check("gt0009", "/assignmentExpressions[1]/variable/code");
        check("gt0010", "/assignmentExpressions[2]/variable/code");
        check("20,kg", "/assignmentExpressions[1]/assignment/value");
        check("1|local::at0028|Present|", "/assignmentExpressions[2]/assignment/value");
    }


    public void testInstanceCreationWithMultipleSetsRoundTrip() throws Exception {
		parseSingleExpression(COMPLEX);
		String actual = item.toString();
		assertEquals(actual, COMPLEX);
	}

	private static final String COMPLEX =
		"$gt0003.create($gt0009=20,kg;$gt0010=1|local::at0028|Present|)";
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