package se.cambio.cds.model.guide.dto;

import java.io.Serializable;
import java.util.Date;

public class GuideDTO implements Serializable{

    private static final long serialVersionUID = 20120412L;
    private String idGuide;
    private String guideSrc;
    private byte[] guideObject;
    private byte[] compiledGuide;
    private boolean active;
    private Date lastUpdate;

    public GuideDTO(String idGuide, String guideSrc, byte[] guideObject, byte[] compiledGuide, boolean active, Date lastUpdate) {
        super();
        this.idGuide = idGuide;
        this.guideSrc = guideSrc;
        this.guideObject = guideObject;
        this.compiledGuide = compiledGuide;
        this.active = active;
        this.lastUpdate = lastUpdate;
    }
    public String getIdGuide() {
        return idGuide;
    }
    public void setIdGuide(String idGuide) {
        this.idGuide = idGuide;
    }
    public String getGuideSrc() {
        return guideSrc;
    }
    public void setGuideSrc(String guideSrc) {
        this.guideSrc = guideSrc;
    }
    public byte[] getGuideObject() {
        return guideObject;
    }
    public void setGuideObject(byte[] guideObject) {
        this.guideObject = guideObject;
    }
    public byte[] getCompiledGuide() {
        return compiledGuide;
    }
    public void setCompiledGuide(byte[] compiledGuide) {
        this.compiledGuide = compiledGuide;
    }
    public boolean isActive() {
        return active;
    }
    public void setActive(boolean active) {
        this.active = active;
    }

    public Date getLastUpdate() {
        return lastUpdate;
    }

    public void setLastUpdate(Date lastUpdate) {
        this.lastUpdate = lastUpdate;
    }
    /*
    public int hashCode() {
        return (guideSrc!=null?guideSrc.hashCode():0)+(guideObject!=null?guideObject.hashCode():2)+(compiledGuide!=null?compiledGuide.hashCode():3)+(active?1:0);
    }
    */
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