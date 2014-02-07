package se.cambio.cds.model.facade.execution.vo;

import java.io.Serializable;

public class RuleReference implements Serializable{

    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String guideId = null;
    private String gtCode = null;
    
    public RuleReference(String ruleId) {
	int index = ruleId.lastIndexOf("/");
	this.guideId = ruleId.substring(0, index);
	this.gtCode = ruleId.substring(index+1);
    }
    
    public RuleReference(String guideId, String gtCode) {
	super();
	this.guideId = guideId;
	this.gtCode = gtCode;
    }
    
    public String getGuideId() {
        return guideId;
    }
    public void setGuideId(String guideId) {
        this.guideId = guideId;
    }
    public String getGTCode() {
        return gtCode;
    }
    public void setGTCode(String gtCode) {
        this.gtCode = gtCode;
    }
    
    public int hashCode(){
	return toString().hashCode();
    }
    
    public boolean equals(Object o){
	return (o!=null && o.hashCode()==this.hashCode()); 
    }
    
    public String toString(){
	return guideId+"/"+gtCode;
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