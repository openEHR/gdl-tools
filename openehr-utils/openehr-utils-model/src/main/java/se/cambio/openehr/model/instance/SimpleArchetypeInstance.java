package se.cambio.openehr.model.instance;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

public class SimpleArchetypeInstance implements Serializable{

    private static final long serialVersionUID = 1L;
    private String archetypeId = null;
    private String templateId = null;
    private Map<String, SimpleElementInstance> elementInstancesMap = null;

    public SimpleArchetypeInstance(String archetypeId, String templateId) {
	this.archetypeId = archetypeId;
	this.templateId = templateId;
	this.elementInstancesMap = new HashMap<String, SimpleElementInstance>();
    }

    protected void addElementInstance(SimpleElementInstance ei){
	this.elementInstancesMap.put(ei.getId(), ei);
    }
    
    protected void removeElementInstance(SimpleElementInstance ei){
	this.elementInstancesMap.remove(ei.getId());
    }

    public Map<String,SimpleElementInstance> getElementInstancesMap(){
	return elementInstancesMap;
    }

    public String getArchetypeId() {
	return archetypeId;
    }

    public void setArchetypeId(String archetypeId) {
	this.archetypeId = archetypeId;
    }

    public String getTemplateId() {
	return templateId;
    }

    public void setIdTemplate(String idTemplate) {
	this.templateId = idTemplate;
    }

    public SimpleArchetypeInstance clone(){
	return new SimpleArchetypeInstance(archetypeId, templateId);
    }

    public String toString(){
	StringBuffer sb = new StringBuffer();
	sb.append(archetypeId);
	if (templateId!=null){
	    sb.append(", "+templateId);
	}
	sb.append("\n");
	for (String idElement : elementInstancesMap.keySet()) {
	    sb.append(elementInstancesMap.get(idElement).toString()+"\n");
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