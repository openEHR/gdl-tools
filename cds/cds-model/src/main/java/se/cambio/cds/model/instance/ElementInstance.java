package se.cambio.cds.model.instance;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.text.DvCodedText;

import java.io.Serializable;

public class ElementInstance implements Serializable{

    private static final long serialVersionUID = 2052012L;
    private String id = null;
    private DataValue dataValue = null;
    private DvCodedText nullFlavour = null;
    private ArchetypeReference archetypeReference = null;
    private ContainerInstance containerInstance = null;

    public ElementInstance(
            String id, DataValue dataValue,
            ArchetypeReference archetypeReference,
            ContainerInstance containerInstance,
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
    public ArchetypeReference getArchetypeReference() {
        return archetypeReference;
    }
    public void setArchetypeReference(ArchetypeReference archetypeReference) {
        if (this.archetypeReference!=null){
            this.archetypeReference.removeElementInstance(this);
        }
        this.archetypeReference = archetypeReference;
        if (archetypeReference!=null){
            archetypeReference.addElementInstance(this);
        }
    }
    public ContainerInstance getContainerInstance() {
        return containerInstance;
    }
    public void setContainerInstance(ContainerInstance containerInstance) {
        this.containerInstance = containerInstance;
    }
    public DataValue getDataValue() {
        return dataValue;
    }
    public void setDataValue(DataValue dataValue) {
        this.dataValue = dataValue;
    }

    /*
    public void setAttribute(String attName, Object value){
	try{
	    Method method = 
		    getDataValue().getClass().getMethod("set"+StringUtils.capitalize(attName), value.getClass());
	    method.invoke(getDataValue(), value);
	}catch(Exception e){
	    ExceptionHandler.handle(e);
	}
    }*/

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

    public ElementInstance clone(){
        return new ElementInstance(id, dataValue, archetypeReference.clone(), containerInstance, nullFlavour);
    }

    public boolean isPredicate(){
        return false;
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