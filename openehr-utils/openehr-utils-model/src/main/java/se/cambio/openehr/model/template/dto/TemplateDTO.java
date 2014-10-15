package se.cambio.openehr.model.template.dto;

import java.io.Serializable;

/**
 * @author iago.corbal
 *
 */
public class TemplateDTO implements Serializable{

    private java.lang.String templateId;
    private java.lang.String arcehtypeId;
    private java.lang.String name;
    private java.lang.String description;
    private java.lang.String rmName;
    private java.lang.String archetype;
    private byte[] aom;
    private byte[] aobcVO;


    private static final long serialVersionUID = 23032012L;

    public TemplateDTO(String templateId, String arcehtypeId,
                       String name, String description,
                       String entryType, String archetype, byte[] aom, byte[] aobcVO) {
        super();
        this.templateId = templateId;
        this.arcehtypeId = arcehtypeId;
        this.name = name;
        this.description = description;
        this.rmName = entryType;
        this.archetype = archetype;
        this.aom = aom;
        this.aobcVO = aobcVO;
    }

    public java.lang.String getTemplateId() {
        return templateId;
    }

    public void setTemplateId(java.lang.String templateId) {
        this.templateId = templateId;
    }

    public java.lang.String getArcehtypeId() {
        return arcehtypeId;
    }

    public void setArcehtypeId(java.lang.String arcehtypeId) {
        this.arcehtypeId = arcehtypeId;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public java.lang.String getRMName() {
        return rmName;
    }

    public void setRMName(java.lang.String entryType) {
        this.rmName = entryType;
    }

    public byte[] getAom() {
        return aom;
    }

    public void setAom(byte[] aom) {
        this.aom = aom;
    }

    public java.lang.String getArchetype() {
        return archetype;
    }

    public void setArchetype(java.lang.String archetype) {
        this.archetype = archetype;
    }

    public byte[] getAobcVO() {
        return aobcVO;
    }

    public void setAobcVO(byte[] aobcVO) {
        this.aobcVO = aobcVO;
    }

    /*
    public int hashCode() {
        return (archetype!=null?archetype.hashCode():0)+(aom!=null?aom.hashCode():1)+(aobcVO!=null?aobcVO.hashCode():2);
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