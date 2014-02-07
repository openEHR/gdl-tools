package se.cambio.openehr.model.archetype.vo;

import java.io.Serializable;

public class PathableVO implements Serializable{

    private static final long serialVersionUID = 25042012L;
    private java.lang.String name;
    private java.lang.String description;
    private java.lang.String type;
    private java.lang.String idParent;
    private java.lang.String idArchetype;
    private java.lang.String idTemplate;
    private java.lang.String path;
    private java.lang.Integer lowerCardinality;
    private java.lang.Integer upperCardinality;
    

    public PathableVO(String name, String description, String type,
	    String idParentCluster, String idArchetype, String idTemplate, String path) {
	super();
	this.name = name;
	this.description = description;
	this.type = type;
	this.idParent = idParentCluster;
	this.idArchetype = idArchetype;
	this.idTemplate = idTemplate;
	this.path = path;
    }

    public java.lang.String getId() {
	return idArchetype+path;
    }

    public java.lang.String getName() {
	return name;
    }
    public void setName(java.lang.String name) {
	this.name = name;
    }
    public java.lang.String getDescription() {
	return description;
    }
    public void setDescription(java.lang.String description) {
	this.description = description;
    }
    public java.lang.String getRMType() {
        return type;
    }
    public void setType(java.lang.String type) {
        this.type = type;
    }
    public java.lang.String getIdParent() {
	return idParent;
    }
    public void setIdParent(java.lang.String idParentCluster) {
	this.idParent = idParentCluster;
    }
    public java.lang.String getIdArchetype() {
	return idArchetype;
    }
    public void setIdArchetype(java.lang.String idArchetype) {
	this.idArchetype = idArchetype;
    }
    public java.lang.String getIdTemplate() {
        return idTemplate;
    }
    public void setIdTemplate(java.lang.String idTemplate) {
        this.idTemplate = idTemplate;
    }
    public java.lang.String getPath() {
	return path;
    }
    public void setPath(java.lang.String path) {
	this.path = path;
    }
    public java.lang.Integer getLowerCardinality() {
        return lowerCardinality;
    }
    public void setLowerCardinality(java.lang.Integer lowerCardinality) {
        this.lowerCardinality = lowerCardinality;
    }
    public java.lang.Integer getUpperCardinality() {
        return upperCardinality;
    }
    public void setUpperCardinality(java.lang.Integer upperCardinality) {
        this.upperCardinality = upperCardinality;
    }
    public java.lang.String getType() {
        return type;
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