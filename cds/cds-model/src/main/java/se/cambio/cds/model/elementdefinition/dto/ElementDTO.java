package se.cambio.cds.model.elementdefinition.dto;

import java.io.Serializable;

public class ElementDTO implements Serializable{

    private static final long serialVersionUID = 20120412L;
    private Integer idElement;
    private String description;
    private String dataType;
    private int idParentElement;
    private String archetype;
    private String path;

    public ElementDTO(Integer idElement, String description,
	    String dataType, int idParentElement, String archetype,
	    String path) {
	super();
	this.idElement = idElement;
	this.description = description;
	this.dataType = dataType;
	this.idParentElement = idParentElement;
	this.archetype = archetype;
	this.path = path;
    }

    public Integer getIdElement() {
	return idElement;
    }

    public void setIdElement(Integer idElement) {
	this.idElement = idElement;
    }

    public String getDescription() {
	return description;
    }

    public void setDescription(String description) {
	this.description = description;
    }

    public String getDataType() {
	return dataType;
    }

    public void setDataType(String dataType) {
	this.dataType = dataType;
    }

    public int getIdParentElement() {
	return idParentElement;
    }

    public void setIdParentElement(int idParentElement) {
	this.idParentElement = idParentElement;
    }

    public String getArchetype() {
	return archetype;
    }

    public void setArchetype(String archetype) {
	this.archetype = archetype;
    }

    public String getPath() {
	return path;
    }

    public void setPath(String path) {
	this.path = path;
    }
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  1.1 (the 'License'); you may not use this file except in compliance with
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