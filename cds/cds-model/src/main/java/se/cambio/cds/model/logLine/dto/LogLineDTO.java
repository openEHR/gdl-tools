package se.cambio.cds.model.logLine.dto;

import java.io.Serializable;

public class LogLineDTO implements Serializable{

    private static final long serialVersionUID = 1L;
    private Integer idLogLine;
    /**
     * 0 - INFO
     * 1 - ERROR
     */
    private Short logType;
    private String msg;
    private String idUser;
    
    public LogLineDTO(Integer idLogLine, Short logType, String msg,
	    String idUser) {
	super();
	this.idLogLine = idLogLine;
	this.logType = logType;
	this.msg = msg;
	this.idUser = idUser;
    }
    
    public Integer getIdLogLine() {
        return idLogLine;
    }
    public void setIdLogLine(Integer idLogLine) {
        this.idLogLine = idLogLine;
    }
    public Short getLogType() {
        return logType;
    }
    public void setLogType(Short logType) {
        this.logType = logType;
    }
    public String getMsg() {
        return msg;
    }
    public void setMsg(String msg) {
        this.msg = msg;
    }
    public String getIdUser() {
        return idUser;
    }
    public void setIdUser(String idUser) {
        this.idUser = idUser;
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