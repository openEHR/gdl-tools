package se.cambio.cds.gdl.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import se.cambio.cds.gdl.model.expression.ExpressionItem;

/**
 * Guide definition object
 * 
 * @author rong.chen
 * 
 */

public class GuideDefinition implements Serializable{

    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    public GuideDefinition() {
    }

    public GuideDefinition(List<ArchetypeBinding> archetypeBindings,
	    List<String> preConditions, Map<String,Rule> rules) {
	super();
	this.archetypeBindings = archetypeBindings;
	this.preConditions = preConditions;
	this.rules = rules;
    }

    /**
     * @return the archetypeBindings
     */
    public List<ArchetypeBinding> getArchetypeBindings() {
	return archetypeBindings;
    }

    /**
     * @return the preConditions, list of pre-conditions joined by default AND operator
     */
    public List<String> getPreConditions() {		
	if(preConditionExpressions == null || preConditionExpressions.size() == 0) {
	    return preConditions;
	}
	List<String> lines = new ArrayList<String>();
	for(ExpressionItem item : preConditionExpressions) {
	    lines.add(item.toString());	
	}
	return lines; 
    }

    /**
     * @return the rules
     */
    public Map<String,Rule> getRules() {
	return rules;
    }

    /**
     * @param archetypeBindings the archetypeBindings to set
     */
    public void setArchetypeBindings(List<ArchetypeBinding> archetypeBindings) {
	this.archetypeBindings = archetypeBindings;
    }

    /**
     * @param preConditions the preConditions to set
     */
    public void setPreConditions(List<String> preConditions) {
	this.preConditions = preConditions;
    }

    /**
     * @param rules the rules to set
     */
    public void setRules(Map<String,Rule> rules) {
	this.rules = rules;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
	final int prime = 31;
	int result = 1;
	result = prime
		* result
		+ ((archetypeBindings == null) ? 0 : archetypeBindings
			.hashCode());
	result = prime * result
		+ ((preConditions == null) ? 0 : preConditions.hashCode());
	result = prime * result + ((rules == null) ? 0 : rules.hashCode());
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
	GuideDefinition other = (GuideDefinition) obj;
	if (archetypeBindings == null) {
	    if (other.archetypeBindings != null)
		return false;
	} else if (!archetypeBindings.equals(other.archetypeBindings))
	    return false;
	if (preConditions == null) {
	    if (other.preConditions != null)
		return false;
	} else if (!preConditions.equals(other.preConditions))
	    return false;
	if (rules == null) {
	    if (other.rules != null)
		return false;
	} else if (!rules.equals(other.rules))
	    return false;
	return true;
    }	

    private List<ArchetypeBinding> archetypeBindings;
    private List<String> preConditions;
    private Map<String,Rule> rules;	
    private List<ExpressionItem> preConditionExpressions;
    /**
     * @return the preConditionExpressions
     */
    public List<ExpressionItem> getPreConditionExpressions() {
	return preConditionExpressions;
    }

    /**
     * @param preConditionExpressions the preConditionExpressions to set
     */
    public void setPreConditionExpressions(
	    List<ExpressionItem> preConditionExpressions) {
	this.preConditionExpressions = preConditionExpressions;
	List<String> lines = new ArrayList<String>();
	if (preConditionExpressions!=null){
	    for(ExpressionItem item : preConditionExpressions) {
		String str = item.toString();
		if (str!=null){
		    lines.add(str);
		}
	    }
	}
	this.preConditions = lines;
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