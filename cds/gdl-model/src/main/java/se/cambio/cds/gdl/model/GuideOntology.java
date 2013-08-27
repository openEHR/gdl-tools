package se.cambio.cds.gdl.model;

import java.io.Serializable;
import java.util.Map;

public class GuideOntology implements Serializable{
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;
	private Map<String, TermDefinition> termDefinitions;
	private Map<String, TermBinding> termBindings;
	/**
	 * @return the termDefinitions
	 */
	public Map<String, TermDefinition> getTermDefinitions() {
		return termDefinitions;
	}
	/**
	 * @param termDefinitions the termDefinitions to set
	 */
	public void setTermDefinitions(Map<String, TermDefinition> termDefinitions) {
		this.termDefinitions = termDefinitions;
	}
	/**
	 * @return the termBindings
	 */
	public Map<String, TermBinding> getTermBindings() {
		return termBindings;
	}
	/**
	 * @param termBindings the termBindings to set
	 */
	public void setTermBindings(Map<String, TermBinding> termBindings) {
		this.termBindings = termBindings;
	}
	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((termBindings == null) ? 0 : termBindings.hashCode());
		result = prime * result
				+ ((termDefinitions == null) ? 0 : termDefinitions.hashCode());
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
		GuideOntology other = (GuideOntology) obj;
		if (termBindings == null) {
			if (other.termBindings != null)
				return false;
		} else if (!termBindings.equals(other.termBindings))
			return false;
		if (termDefinitions == null) {
			if (other.termDefinitions != null)
				return false;
		} else if (!termDefinitions.equals(other.termDefinitions))
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