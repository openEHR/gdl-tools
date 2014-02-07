package se.cambio.openehr.model.archetype.dto;

import java.io.Serializable;

/**
 * @author iago.corbal
 *
 */
public class ArchetypeDTO implements Serializable{

    private java.lang.String idArchetype;
    private java.lang.String name;
    private java.lang.String description;
    private java.lang.String rmName;
    private java.lang.String archetype;
    private byte[] aom;
    private byte[] aobcVO;

    private static final long serialVersionUID = 23032012L;

    public ArchetypeDTO(String idArchetype, String name, String description,
                        String entryType, String archetype, byte[] aom, byte[] aobcVO) {
        super();
        this.idArchetype = idArchetype;
        this.name = name;
        this.description = description;
        this.rmName = entryType;
        this.archetype = archetype;
        this.aom = aom;
        this.aobcVO = aobcVO;
    }

    public java.lang.String getIdArchetype() {
        return idArchetype;
    }

    public void setIdArchetype(java.lang.String idArchetype) {
        this.idArchetype = idArchetype;
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