package se.cambio.cm.model.guide.dto;

import se.cambio.cm.model.util.CMElement;

import java.util.Date;

public class GuideDTO implements CMElement {

    private static final long serialVersionUID = 20120412L;
    private String id;
    private String format;
    private String source;
    private byte[] guideObject;
    private byte[] compiledGuide;
    private Date lastUpdate;

    public GuideDTO() {

    }

    public GuideDTO(String id, String format, String source, byte[] guideObject, byte[] compiledGuide, Date lastUpdate) {
        super();
        this.id = id;
        this.format = format;
        this.source = source;
        this.guideObject = guideObject;
        this.compiledGuide = compiledGuide;
        this.lastUpdate = lastUpdate;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    @Override
    public String getFormat() {
        return format;
    }

    @Override
    public void setFormat(String format) {
        this.format = format;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
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

    public Date getLastUpdate() {
        return lastUpdate;
    }

    public void setLastUpdate(Date lastUpdate) {
        this.lastUpdate = lastUpdate;
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