package se.cambio.openehr.model.archetype.vo;

import java.io.Serializable;

public class ProportionTypeVO implements Serializable{

    private static final long serialVersionUID = 25052012L;
    private String idTemplate = null;
    private String idElement = null;
    private Integer type = null;
    public ProportionTypeVO(String idTemplate, String idElement, Integer type) {
	super();
	this.idTemplate = idTemplate;
	this.idElement = idElement;
	this.type = type;
    }
    public String getIdTemplate() {
        return idTemplate;
    }
    public void setIdTemplate(String idTemplate) {
        this.idTemplate = idTemplate;
    }
    public String getIdElement() {
        return idElement;
    }
    public void setIdElement(String idElement) {
        this.idElement = idElement;
    }
    public Integer getType() {
        return type;
    }
    public void setType(Integer type) {
        this.type = type;
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