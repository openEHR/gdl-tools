package se.cambio.cds.gdl.model.expression;

import java.util.*;

/**
 * This represents an expression that use built-in functions with optionally a
 * list of variables
 * 
 * @author rong.chen
 */
public class FunctionalExpression extends ExpressionItem {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;
	public static FunctionalExpression create(String function) {
		return new FunctionalExpression(function);
	}
	
	public static FunctionalExpression create(String function, ExpressionItem item) {
		List<ExpressionItem> items = new ArrayList<ExpressionItem>();
		items.add(item);
		return new FunctionalExpression(function, items);
	}
	
	public static FunctionalExpression create(String function, List<ExpressionItem> items) {
		return new FunctionalExpression(function, items);
	}
	
	public FunctionalExpression(String function) {
		this(function, null);
	}
	public FunctionalExpression(String function, List<ExpressionItem> items) {
		super();
		
		if(function == null || function.isEmpty()) {
			throw new IllegalArgumentException("null or empty function name");
		}
		this.function = function;
		
		if(items != null) {
			this.items = new ArrayList<ExpressionItem>(items);
		}
	}

	/**
	 * @return the function
	 */
	public String getFunction() {
		return function;
	}
	
	
	/**
	 * String representation of this expression using the following format:
	 * 
	 * function() without any variables
	 * 
	 * or
	 * 
	 * function(var1, var2..)
	 * 
	 */
	public String toString() {
		StringBuffer buf = new StringBuffer();
		buf.append(function);
		buf.append("(");
		ExpressionItem item  = null;
		if(items != null) {
			for(int i = 0, j = items.size(); i < j; i++) {
				item = items.get(i);
				if(item instanceof BinaryExpression) {
					buf.append("(");
					buf.append(item.toString());
					buf.append(")");
				} else {
					buf.append(item.toString());
				}
				if(i != j -1) {
					buf.append(",");
				}
			}
		}
		buf.append(")");
		return buf.toString();
	}

	/**
	 * @return the items
	 */
	public List<ExpressionItem> getItems() {
		return Collections.unmodifiableList(items);
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((function == null) ? 0 : function.hashCode());
		result = prime * result + ((items == null) ? 0 : items.hashCode());
		return result;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		FunctionalExpression other = (FunctionalExpression) obj;
		if (function == null) {
			if (other.function != null)
				return false;
		} else if (!function.equals(other.function))
			return false;
		if (items == null) {
			if (other.items != null)
				return false;
		} else if (!items.equals(other.items))
			return false;
		return true;
	}

	private String function;
	private List<ExpressionItem> items;
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