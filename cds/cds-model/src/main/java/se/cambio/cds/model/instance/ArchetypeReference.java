package se.cambio.cds.model.instance;

import lombok.Data;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

@Data
public class ArchetypeReference implements Serializable, Cloneable {

    private static final long serialVersionUID = 1L;
    private String idDomain = null;
    private String idArchetype = null;
    private String idTemplate = null;
    private Map<String, ElementInstance> elementInstancesMap = null;

    public ArchetypeReference(String idDomain, String idArchetype, String idTemplate) {
        this.idDomain = idDomain;
        this.idArchetype = idArchetype;
        this.idTemplate = idTemplate;
    }

    protected void addElementInstance(ElementInstance ei) {
        getElementInstancesMap().put(ei.getId(), ei);
    }

    protected void removeElementInstance(ElementInstance ei) {
        getElementInstancesMap().remove(ei.getId());
    }

    public Map<String, ElementInstance> getElementInstancesMap() {
        if (elementInstancesMap == null) {
            elementInstancesMap = new HashMap<>();
        }
        return elementInstancesMap;
    }

    @Override
    public ArchetypeReference clone() {
        try {
            ArchetypeReference clone = (ArchetypeReference) super.clone();
            clone.setIdArchetype(idArchetype);
            clone.setIdTemplate(idTemplate);
            clone.setIdDomain(idDomain);
            return clone;
        } catch (CloneNotSupportedException ex) {
            throw new RuntimeException(ex);
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(idArchetype).append(", ").append(idDomain);
        if (idTemplate != null) {
            sb.append(", " + idTemplate);
        }
        sb.append("\n");
        for (String idElement : getElementInstancesMap().keySet()) {
            sb.append(getElementInstancesMap().get(idElement).toString() + "\n");
        }
        return sb.toString();
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