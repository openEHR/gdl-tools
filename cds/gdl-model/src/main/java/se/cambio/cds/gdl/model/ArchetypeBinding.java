package se.cambio.cds.gdl.model;

import se.cambio.cds.gdl.model.expression.ExpressionItem;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class ArchetypeBinding  implements Serializable{

	/**
     * 
     */
    private static final long serialVersionUID = 1L;

	public ArchetypeBinding() {
	}

	private String archetypeId;
	private String templateId;
	private String domain;
	//private String function;
	private String timeRange;
	private Map<String, ElementBinding> elements;
	private List<String> predicates;
	private List<ExpressionItem> predicateStatements;

	/**
	 * @return the archetypeId
	 */
	public String getArchetypeId() {
		return archetypeId;
	}

	/**
	 * @param archetypeId
	 *            the archetypeId to set
	 */
	public void setArchetypeId(String archetypeId) {
		this.archetypeId = archetypeId;
	}

	/**
	 * @return the templateId
	 */
	public String getTemplateId() {
		return templateId;
	}

	/**
	 * @param templateId
	 *            the templateId to set
	 */
	public void setTemplateId(String templateId) {
		this.templateId = templateId;
	}

	/**
	 * @return the domain
	 */
	public String getDomain() {
		return domain;
	}

	/**
	 * @param domain
	 *            the domain to set
	 */
	public void setDomain(String domain) {
		this.domain = domain;
	}


	/**
	 * @return the function
	 */
    /*
	public String getFunction() {
		return function;
	} */

	/**
	 * @param function
	 *            the function to set
	 */
    /*
	public void setFunction(String function) {
		this.function = function;
	} */

	/**
	 * @return the timeRange
	 */
	public String getTimeRange() {
		return timeRange;
	}

	/**
	 * @param timeRange
	 *            the timeRange to set
	 */
	public void setTimeRange(String timeRange) {
		this.timeRange = timeRange;
	}

	/**
	 * @return the elements
	 */
	public Map<String, ElementBinding> getElements() {
		return elements;
	}

	/**
	 * @param elements
	 *            the elements to set
	 */
	public void setElements(Map<String, ElementBinding> elements) {
		this.elements = elements;
	}

	/**
	 * @return the predicates
	 */
	public List<String> getPredicates() {
		if(predicateStatements == null || predicateStatements.size() == 0) {
			return predicates;
		}
		List<String> lines = new ArrayList<String>();
		for(ExpressionItem item : predicateStatements) {
			lines.add(item.toString());	
		}
		return lines; 
	}

	/**
	 * @param predicates
	 *            the predicates to set
	 */
	public void setPredicates(List<String> predicates) {
		this.predicates = predicates;
	}

	/**
	 * @return the predicateStatements
	 */
	public List<ExpressionItem> getPredicateStatements() {
		return predicateStatements;
	}

	/**
	 * @param predicateStatements
	 *            the predicateStatements to set
	 */
	public void setPredicateStatements(List<ExpressionItem> predicateStatements) {
		this.predicateStatements = predicateStatements;
		List<String> lines = new ArrayList<String>();
		if (predicateStatements!=null){
		    for(ExpressionItem item : predicateStatements) {
			lines.add(item.toString());
		    }
		}
		this.predicates = lines;
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
		result = prime * result
				+ ((archetypeId == null) ? 0 : archetypeId.hashCode());
		result = prime * result + ((domain == null) ? 0 : domain.hashCode());
		result = prime * result
				+ ((elements == null) ? 0 : elements.hashCode());
        /*
		result = prime * result
				+ ((function == null) ? 0 : function.hashCode());   */
		result = prime
				* result
				+ ((predicateStatements == null) ? 0 : predicateStatements
						.hashCode());
		result = prime * result
				+ ((predicates == null) ? 0 : predicates.hashCode());
		result = prime * result
				+ ((templateId == null) ? 0 : templateId.hashCode());
		result = prime * result
				+ ((timeRange == null) ? 0 : timeRange.hashCode());
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
		ArchetypeBinding other = (ArchetypeBinding) obj;
		if (archetypeId == null) {
			if (other.archetypeId != null)
				return false;
		} else if (!archetypeId.equals(other.archetypeId))
			return false;
		if (domain == null) {
			if (other.domain != null)
				return false;
		} else if (!domain.equals(other.domain))
			return false;
		if (elements == null) {
			if (other.elements != null)
				return false;
		} else if (!elements.equals(other.elements))
			return false;
        /*
		if (function == null) {
			if (other.function != null)
				return false;
		} else if (!function.equals(other.function))
			return false;
			*/
		if (predicates == null) {
			if (other.predicates != null)
				return false;
		} else if (!predicates.equals(other.predicates))
			return false;
		if (templateId == null) {
			if (other.templateId != null)
				return false;
		} else if (!templateId.equals(other.templateId))
			return false;
		if (timeRange == null) {
			if (other.timeRange != null)
				return false;
		} else if (!timeRange.equals(other.timeRange))
			return false;
		return true;
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