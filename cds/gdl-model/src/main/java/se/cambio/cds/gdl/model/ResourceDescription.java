package se.cambio.cds.gdl.model;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * ResourceDescription
 * 
 * @author rong.chen
 */
public class ResourceDescription implements Serializable{

	/**
     * 
     */
    private static final long serialVersionUID = 1L;
	private Map<String, String> originalAuthor;
	private List<String> otherContributors;
	private String lifecycleState;
	private Map<String, ResourceDescriptionItem> details;
	private String resourcePackageUri;
	private Map<String, String> otherDetails;

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((details == null) ? 0 : details.hashCode());
		result = prime * result
				+ ((lifecycleState == null) ? 0 : lifecycleState.hashCode());
		result = prime * result
				+ ((originalAuthor == null) ? 0 : originalAuthor.hashCode());
		result = prime
				* result
				+ ((otherContributors == null) ? 0 : otherContributors
						.hashCode());
		result = prime * result
				+ ((otherDetails == null) ? 0 : otherDetails.hashCode());
		result = prime
				* result
				+ ((resourcePackageUri == null) ? 0 : resourcePackageUri
						.hashCode());
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
		ResourceDescription other = (ResourceDescription) obj;
		if (details == null) {
			if (other.details != null)
				return false;
		} else if (!details.equals(other.details))
			return false;
		if (lifecycleState == null) {
			if (other.lifecycleState != null)
				return false;
		} else if (!lifecycleState.equals(other.lifecycleState))
			return false;
		if (originalAuthor == null) {
			if (other.originalAuthor != null)
				return false;
		} else if (!originalAuthor.equals(other.originalAuthor))
			return false;
		if (otherContributors == null) {
			if (other.otherContributors != null)
				return false;
		} else if (!otherContributors.equals(other.otherContributors))
			return false;
		if (otherDetails == null) {
			if (other.otherDetails != null)
				return false;
		} else if (!otherDetails.equals(other.otherDetails))
			return false;
		if (resourcePackageUri == null) {
			if (other.resourcePackageUri != null)
				return false;
		} else if (!resourcePackageUri.equals(other.resourcePackageUri))
			return false;
		return true;
	}

	/**
	 * @return the originalAuthor
	 */
	public Map<String, String> getOriginalAuthor() {
		return originalAuthor;
	}

	/**
	 * @param originalAuthor
	 *            the originalAuthor to set
	 */
	public void setOriginalAuthor(Map<String, String> originalAuthor) {
		this.originalAuthor = originalAuthor;
	}

	/**
	 * @return the otherContributors
	 */
	public List<String> getOtherContributors() {
		return otherContributors;
	}

	/**
	 * @param otherContributors
	 *            the otherContributors to set
	 */
	public void setOtherContributors(List<String> otherContributors) {
		this.otherContributors = otherContributors;
	}

	/**
	 * @return the lifecycleState
	 */
	public String getLifecycleState() {
		return lifecycleState;
	}

	/**
	 * @param lifecycleState
	 *            the lifecycleState to set
	 */
	public void setLifecycleState(String lifecycleState) {
		this.lifecycleState = lifecycleState;
	}

	/**
	 * @return the details
	 */
	public Map<String, ResourceDescriptionItem> getDetails() {
		return details;
	}

	/**
	 * @param details
	 *            the details to set
	 */
	public void setDetails(Map<String, ResourceDescriptionItem> details) {
		this.details = details;
	}

	/**
	 * @return the resourcePackageUri
	 */
	public String getResourcePackageUri() {
		return resourcePackageUri;
	}

	/**
	 * @param resourcePackageUri
	 *            the resourcePackageUri to set
	 */
	public void setResourcePackageUri(String resourcePackageUri) {
		this.resourcePackageUri = resourcePackageUri;
	}

	/**
	 * @return the otherDetails
	 */
	public Map<String, String> getOtherDetails() {
		return otherDetails;
	}

	/**
	 * @param otherDetails
	 *            the otherDetails to set
	 */
	public void setOtherDetails(Map<String, String> otherDetails) {
		this.otherDetails = otherDetails;
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