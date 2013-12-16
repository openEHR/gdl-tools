package se.cambio.cds.gdl.model.expression;

public enum OperatorKind {
	
	/**
	 * Arithmetic operators
	 */
	ADDITION("addition", "+"),
	SUBSTRATION("substraction", "-"),
	MULTIPLICATION("multiplication", "*"),
	DIVISION("division", "/"),
	EXPONENT("exponent", "^"),
	
	/**
	 * Logical operators
	 */
	AND("and", "&&"),
	OR("or", "||"),
	NOT("not", "!"),
	
	/**
	 * Relational operators
	 */
	EQUALITY("equal", "=="),
    INEQUAL("unequal", "!="),
	LESS_THAN("less than", "<"),
	LESS_THAN_OR_EQUAL("less than or equals", "<="),
	GREATER_THAN("greater than", ">"),
	GREATER_THAN_OR_EQUAL("greater than or equals", ">="),
	
	/** 
	 * Assignment operator
	 */
	ASSIGNMENT("assignment", "="),
	
	/**
	 * Terminological reasoning
	 */
	IS_A("is_a", "is_a"),
	IS_NOT_A("is_not_a", "!is_a"),
	
	/**
	 * Conditional operators
	 */
	FOR_ALL("for all","for_all"),
    MAX("max","max"),
    MIN("min","min");
	
	private OperatorKind(String name, String symbol) {
		this.name = name;
		this.symbol = symbol;
	}
	
	public String getName() {
		return name;
	}
	
	public String getSymbol() {
		return symbol;
	}
	
	public String toString() {
		return name;
	}
	
	private String name;
	private String symbol;
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