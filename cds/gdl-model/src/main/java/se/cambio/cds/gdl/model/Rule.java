package se.cambio.cds.gdl.model;

import java.io.Serializable;
import java.util.*;

import se.cambio.cds.gdl.model.expression.AssignmentExpression;
import se.cambio.cds.gdl.model.expression.ExpressionItem;

public class Rule implements Serializable{

	/**
     * 
     */
    private static final long serialVersionUID = 1L;

	public Rule() {
	}

	private String id; // gt-code
	private List<String> when;
	private List<String> then;
	private int priority;

	private List<ExpressionItem> whenStatements;
	private List<AssignmentExpression> thenStatements;

	/**
	 * @return the id
	 */
	public String getId() {
		return id;
	}

	/**
	 * @param id
	 *            the id to set
	 */
	public void setId(String id) {
		this.id = id;
	}

	/**
	 * @return the when
	 */
	public List<String> getWhen() {
		if(whenStatements == null || whenStatements.size() == 0) {
			return when;
		}
		List<String> lines = new ArrayList<String>();
		for(ExpressionItem item : whenStatements) {
			lines.add(item.toString());
		}
		return lines; 
	}

	/**
	 * @param when
	 *            the when to set
	 */
	public void setWhen(List<String> when) {
		this.when = when;
	}

	/**
	 * @return the then
	 */
	public List<String> getThen() {
		if(thenStatements == null || thenStatements.size() == 0) {
			return then;
		}
		List<String> lines = new ArrayList<String>();
		for(ExpressionItem item : thenStatements) {
			lines.add(item.toString());
		}
		return lines; 
	}

	/**
	 * @param then
	 *            the then to set
	 */
	public void setThen(List<String> then) {
		this.then = then;
	}

	/**
	 * @return the whenStatements
	 */
	public List<ExpressionItem> getWhenStatements() {
		return whenStatements;
	}

	/**
	 * @param whenStatements
	 *            the whenStatements to set
	 */
	public void setWhenStatements(List<ExpressionItem> whenStatements) {
		this.whenStatements = whenStatements;
		List<String> lines = new ArrayList<String>();
		if (this.whenStatements!=null){
		    for(ExpressionItem item : whenStatements) {
			lines.add(item.toString());
		    }
		}
		this.when = lines;
	}

	/**
	 * @return the thenStatements
	 */
	public List<AssignmentExpression> getThenStatements() {
		return thenStatements;
	}

	/**
	 * @param thenStatements
	 *            the thenStatements to set
	 */
	public void setThenStatements(List<AssignmentExpression> thenStatements) {
		this.thenStatements = thenStatements;
		List<String> lines = new ArrayList<String>();
		if (this.thenStatements!=null){
		    for(ExpressionItem item : thenStatements) {
			lines.add(item.toString());
		    }
		}
		this.then = lines;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((id == null) ? 0 : id.hashCode());
		result = prime * result + ((then == null) ? 0 : then.hashCode());
		result = prime * result
				+ ((thenStatements == null) ? 0 : thenStatements.hashCode());
		result = prime * result + ((when == null) ? 0 : when.hashCode());
		result = prime * result
				+ ((whenStatements == null) ? 0 : whenStatements.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
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
		Rule other = (Rule) obj;
		if (id == null) {
			if (other.id != null)
				return false;
		} else if (!id.equals(other.id))
			return false;
		if (then == null) {
			if (other.then != null)
				return false;
		} else if (!then.equals(other.then))
			return false;
		if (thenStatements == null) {
			if (other.thenStatements != null)
				return false;
		} else if (!thenStatements.equals(other.thenStatements))
			return false;
		
		// comparing string representation of when statements causes problems
		// in round-trip testcases
		/*if (when == null) {
			if (other.when != null)
				return false;
		} else if (!when.equals(other.when))
			return false;*/		
		
		if (whenStatements == null) {
			if (other.whenStatements != null)
				return false;
		} else if (!whenStatements.equals(other.whenStatements))
			return false;
		return true;
	}

	/**
	 * @return the priority
	 */
	public int getPriority() {
		return priority;
	}

	/**
	 * @param priority the priority to set
	 */
	public void setPriority(int priority) {
		this.priority = priority;
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