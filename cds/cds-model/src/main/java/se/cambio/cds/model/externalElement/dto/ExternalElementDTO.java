package se.cambio.cds.model.externalElement.dto;

import java.io.Serializable;

public class ExternalElementDTO implements Serializable{

    private static final long serialVersionUID = 20120412L;
    private String idExternalElement;
    private String idDataSource;
    private Integer idElement;

    public ExternalElementDTO(String idExternalElement,
	    String idDataSource, Integer idElement) {
	super();
	this.idExternalElement = idExternalElement;
	this.idDataSource = idDataSource;
	this.idElement = idElement;
    }
    public String getIdExternalElement() {
	return idExternalElement;
    }
    public void setIdExternalElement(String idExternalElement) {
	this.idExternalElement = idExternalElement;
    }
    public String getIdDataSource() {
	return idDataSource;
    }
    public void setIdDataSource(String idDataSource) {
	this.idDataSource = idDataSource;
    }
    public Integer getIdElement() {
	return idElement;
    }
    public void setIdElement(Integer idElement) {
	this.idElement = idElement;
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