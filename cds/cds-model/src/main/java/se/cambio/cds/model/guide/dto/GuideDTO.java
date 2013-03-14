package se.cambio.cds.model.guide.dto;

import java.io.Serializable;

public class GuideDTO implements Serializable{

    private static final long serialVersionUID = 20120412L;
    private String idGuide;
    private String name;
    private String description;
    private String guideSrc;
    private byte[] compiledGuide;

    public GuideDTO(String idGuide, String name, String description, String guideSrc, byte[] compiledGuide) {
	super();
	this.idGuide = idGuide;
	this.name = name;
	this.description = description;
	this.guideSrc = guideSrc;
	this.compiledGuide = compiledGuide;
    }
    public String getIdGuide() {
	return idGuide;
    }
    public void setIdGuide(String idGuide) {
	this.idGuide = idGuide;
    }
    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }
    public String getDescription() {
        return description;
    }
    public void setDescription(String description) {
        this.description = description;
    }
    public String getGuideSrc() {
	return guideSrc;
    }
    public void setGuideSrc(String guideSrc) {
	this.guideSrc = guideSrc;
    }
    public byte[] getCompiledGuide() {
	return compiledGuide;
    }
    public void setCompiledGuide(byte[] compiledGuide) {
	this.compiledGuide = compiledGuide;
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