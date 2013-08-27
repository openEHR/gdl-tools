package se.cambio.cds.gdl.parser;


public class DataValueExpressionTest extends ExpressionTestBase {
	
	public void testParseCodedTextAssignment() throws Exception {
		parseSingleExpression("$gt0001=SNOMED-CT::313267000|Stroke|");	
		check("SNOMED-CT", "/assignment/codedText/definingCode/terminologyId/value");
		check("313267000", "/assignment/codedText/definingCode/codeString");
		check("Stroke", "/assignment/codedText/value");
		check("gt0001", "/variable/code");
	}	
	
	public void testParseDvOrdinalAssignment() throws Exception {
		parseSingleExpression("$gt0001=1|SNOMED-CT::313267000|Stroke|");	
		check(1, "/assignment/ordinal/value");
		check("SNOMED-CT", "/assignment/ordinal/symbol/definingCode/terminologyId/value");
		check("313267000", "/assignment/ordinal/symbol/definingCode/codeString");
		check("Stroke", "/assignment/ordinal/symbol/value");
		check("gt0001", "/variable/code");
	}	
	
	public void testParseCodedTextWithCommaInLabel() throws Exception {
		parseSingleExpression("$gt0005==local::at0010|Fully clothed, including shoes|");	
		check("Fully clothed, including shoes", "/right/codedText/value");
	}

	public void testParseCodedTextWithSlashInLabel() throws Exception {
		parseSingleExpression("$gt0005==local::at0017|Nappy/diaper|");	
		check("Nappy/diaper", "/right/codedText/value");
	}
	
	public void testParseCodePhraseAssignment() throws Exception {
		parseSingleExpression("$gt0001=SNOMED-CT::313267000");	
		check("SNOMED-CT", "/assignment/codePhrase/terminologyId/value");
		check("313267000", "/assignment/codePhrase/codeString");
		check("gt0001", "/variable/code");
	}
	
	public void testParseQuantityWithSpecialUnits() throws Exception {
		parseSingleExpression("$gt0002>56,mm[Hg]" );		
		check(56.0, "/right/quantity/magnitude");
		check("mm[Hg]", "/right/quantity/units");
		check("gt0002", "/left/code");
	}

	public void testParseQuantityUnitsWithSlash() throws Exception {
		parseSingleExpression("$gt0017<3,mmol/l" );		
		check(3.0, "/right/quantity/magnitude");
		check("mmol/l", "/right/quantity/units");
		check("gt0017", "/left/code");
	}
	
	public void testParseProportion() throws Exception {
		parseSingleExpression("$gt0017<3,100,1" );		
		check("3,100,1", "/right/value");
	}
	
	public void testParseProportion2() throws Exception {
		parseSingleExpression("$gt0017<3.2,100.1,1" );		
		check("3.2,100.1,1", "/right/value");
	}
	
	public void testParseQuantityUnitsWithSlashAsFirstChar() throws Exception {
		parseSingleExpression("$gt0027<=90,/min" );		
		check(90.0, "/right/quantity/magnitude");
		check("/min", "/right/quantity/units");
		check("gt0027", "/left/code");
	}
	
	public void testParseQuantityUnitsWithDegree() throws Exception {
		parseSingleExpression("$gt0012>=36.1,°C" );		
		check(36.1, "/right/quantity/magnitude");
		check("°C", "/right/quantity/units");
		check("gt0012", "/left/code");
	}
	
	public void testParseQuantityUnitsWithPercentage() throws Exception {
		parseSingleExpression("$gt0037>=96,%" );		
		check(96.0, "/right/quantity/magnitude");
		check("%", "/right/quantity/units");
		check("gt0037", "/left/code");
	}
	
	public void testParseQuantityWithUnitsAssignment() throws Exception {
		parseSingleExpression("$gt0001=60,kg");		
		check(60.0, "/assignment/quantity/magnitude");
		check("kg", "/assignment/quantity/units");
		check("gt0001", "/variable/code");
	}

	public void testParseRealQuantityWithUnitsAssignment() throws Exception {
		parseSingleExpression("$gt0001=60.5,kg");		
		check(60.5, "/assignment/quantity/magnitude");
		check("kg", "/assignment/quantity/units");
		check("gt0001", "/variable/code");
	}
	
	public void testParseIntegerQuantityWithUnitsAssignment() throws Exception {
		parseSingleExpression("$gt0001=35,kg");		
		check(35.0, "/assignment/quantity/magnitude");
		check("kg", "/assignment/quantity/units");
		check("gt0001", "/variable/code");
	}
	
	
	public void testParseCurrentTimeVariable() throws Exception {
		parseSingleExpression("$gt0001=$currentDateTime");		
		check("currentDateTime", "/assignment/code");
		check("gt0001", "/variable/code");
	}
	
	public void testParseCurrentTimeVariableWithAttribute() throws Exception {
		parseSingleExpression("$gt0001=$currentDateTime.year");		
		check("currentDateTime", "/assignment/code");
		check("year", "/assignment/attribute");
		check("gt0001", "/variable/code");
	}
	
	public void testParseDvDateTime() throws Exception {
		parseSingleExpression("$gt0002>=(2013-02-20T13:14:05+01:00)");	
		check("2013-02-20T13:14:05+01:00", "/right/value");
	}
	
	public void testParseDateTimeEvaluation() throws Exception {
		parseSingleExpression("$gt0006>(2013-02-20T11:24:18)" );		
		check("2013-02-20T11:24:18", "/right/value");
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