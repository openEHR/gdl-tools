package se.cambio.openehr.model.instance;

import java.io.Serializable;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.text.DvCodedText;

public class SimpleElementInstance implements Serializable{

    private static final long serialVersionUID = 2052012L;
    private String id = null;
    private DataValue dataValue = null;
    private DvCodedText nullFlavour = null;
    private SimpleArchetypeInstance archetypeReference = null;
    private SimpleContainerInstance containerInstance = null;

    public SimpleElementInstance(
	    String id, DataValue dataValue, 
	    SimpleArchetypeInstance archetypeReference,
	    SimpleContainerInstance containerInstance,
	    DvCodedText nullFlavour) {
	super();
	this.id = id;
	this.dataValue = dataValue;
	this.nullFlavour = nullFlavour;
	this.containerInstance = containerInstance;
	setArchetypeReference(archetypeReference);
    }
    public String getId() {
	return id;
    }
    public void setId(String id) {
	this.id = id;
    }
    public SimpleArchetypeInstance getArchetypeReference() {
	return archetypeReference;
    }
    public void setArchetypeReference(SimpleArchetypeInstance archetypeReference) {
	if (this.archetypeReference!=null){
	    this.archetypeReference.removeElementInstance(this);
	}
	this.archetypeReference = archetypeReference;
	if (archetypeReference!=null){
	    archetypeReference.addElementInstance(this);
	}
    }
    public SimpleContainerInstance getContainerInstance() {
	return containerInstance;
    }
    public void setContainerInstance(SimpleContainerInstance containerInstance) {
	this.containerInstance = containerInstance;
    }
    public DataValue getDataValue() {
	return dataValue;
    }
    public void setDataValue(DataValue dataValue) {
	this.dataValue = dataValue;
    }

    public DvCodedText getNullFlavour() {
	return nullFlavour;
    }

    public void setNullFlavour(DvCodedText nullFlavour) {
	this.nullFlavour = nullFlavour;
    }

    public boolean hasValue(){
	return this.dataValue!=null;
    }

    public boolean hasNoValue(){
	return this.dataValue==null;
    }

    public SimpleElementInstance clone(){
	return new SimpleElementInstance(id, dataValue, archetypeReference.clone(), containerInstance, nullFlavour);
    }

    public String toString(){
	return "id="+id+"\n"+
		"dataValue="+dataValue+"\n"+
		"containerInstance="+containerInstance+"\n"+
		"nullFlavour="+nullFlavour;
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