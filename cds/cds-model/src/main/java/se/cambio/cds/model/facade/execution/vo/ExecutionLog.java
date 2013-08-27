package se.cambio.cds.model.facade.execution.vo;

import java.io.Serializable;


public class ExecutionLog implements Serializable{

    /**
     * 
     */
    private static final long serialVersionUID = 30072012L;
    private String firedRule = null;
    private String idTemplate = null;
    private String idElement = null;
    private String dataValueStr = null;

    public ExecutionLog(String firedRule, String idTemplate, String idElement, String dataValueStr) {
	super();
	this.firedRule = firedRule;
	this.idTemplate = idTemplate;
	this.idElement = idElement;
	this.dataValueStr = dataValueStr;
    }
    public String getFiredRule() {
        return firedRule;
    }
    public void setFiredRule(String firedRule) {
        this.firedRule = firedRule;
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
    public String getDataValueStr() {
        return dataValueStr;
    }
    public void setDataValueStr(String dataValueStr) {
        this.dataValueStr = dataValueStr;
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