package se.cambio.cds.model.facade.execution.vo;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

public class ArchetypeReference implements Serializable{

    private static final long serialVersionUID = 1L;
    private String idDomain = null;
    private String idArchetype = null;
    private String idTemplate = null;
    private String aggregationFunction = null;
    private Map<String, ElementInstance> elementInstancesMap = null;

    public ArchetypeReference(String idDomain, String idArchetype, String idTemplate, String aggregationFunction) {
	this.idDomain = idDomain;
	this.idArchetype = idArchetype;
	this.idTemplate = idTemplate;
	this.aggregationFunction = aggregationFunction;
	this.elementInstancesMap = new HashMap<String, ElementInstance>();
    }

    protected void addElementInstanceToMap(ElementInstance ei){
	this.elementInstancesMap.put(ei.getId(), ei);
    }

    public Map<String,ElementInstance> getElementInstancesMap(){
	return elementInstancesMap;
    }

    public String getIdDomain() {
	return idDomain;
    }

    public void setIdDomain(String idDomain) {
	this.idDomain = idDomain;
    }

    public String getIdArchetype() {
	return idArchetype;
    }

    public void setIdArchetype(String idArchetype) {
	this.idArchetype = idArchetype;
    }

    public String getIdTemplate() {
	return idTemplate;
    }

    public void setIdTemplate(String idTemplate) {
	this.idTemplate = idTemplate;
    }

    public String getAggregationFunction() {
	return aggregationFunction;
    }

    public void setAggregationFunction(String aggregationFunction) {
	this.aggregationFunction = aggregationFunction;
    }

    public ArchetypeReference clone(){
	return new ArchetypeReference(idDomain, idArchetype, idTemplate, aggregationFunction);
    }

    public String toString(){
	StringBuffer sb = new StringBuffer();
	sb.append(idArchetype+", "+idDomain);
	if (idTemplate!=null){
	    sb.append(", "+idTemplate);
	}
	if(aggregationFunction!=null){
	    sb.append(", "+aggregationFunction);
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