package se.cambio.cds.model.codedText.dto;

import java.io.Serializable;

public class CodedTextDTO implements Serializable{

    private static final long serialVersionUID = 20120412L;

	private Integer idElement = null;
	private String text = null;
	private String description = null;
	private String index = null;
	private String archTerm = null;

	public CodedTextDTO(Integer idElement, String text, String description,
			String index, String archTerm) {
		super();
		this.idElement = idElement;
		this.text = text;
		this.description = description;
		this.index = index;
		this.archTerm = archTerm;
	}
	public Integer getIdElement() {
		return idElement;
	}
	public void setIdElement(Integer idElement) {
		this.idElement = idElement;
	}
	public String getText() {
		return text;
	}
	public void setText(String text) {
		this.text = text;
	}
	public String getDescription() {
		return description;
	}
	public void setDescription(String description) {
		this.description = description;
	}
	public String getIndex() {
		return index;
	}
	public void setIndex(String index) {
		this.index = index;
	}
	public String getArchTerm() {
		return archTerm;
	}
	public void setArchTerm(String archTerm) {
		this.archTerm = archTerm;
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