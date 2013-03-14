package se.cambio.cds.model.facade.execution.vo;

import java.io.Serializable;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.DvOrdinal;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;

import se.cambio.cds.ts.InvalidCodeException;
import se.cambio.cds.ts.UnsupportedTerminologyException;
import se.cambio.cds.util.CDSTerminologyService;
import se.cambio.cds.util.DVUtil;
import se.cambio.cds.util.handlers.ExceptionHandler;

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
	setArchetypeReference(archetypeReference);
	this.dataValue = dataValue;
	this.nullFlavour = nullFlavour;
	this.containerInstance = containerInstance;
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
	this.archetypeReference = archetypeReference;
	if (archetypeReference!=null){
	    archetypeReference.addElementInstanceToMap(this);
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

    public void setAttribute(String attName, Object value){
	try{
	    Method method = 
		    getDataValue().getClass().getMethod("set"+StringUtils.capitalize(attName), value.getClass());
	    method.invoke(getDataValue(), value);
	}catch(Exception e){
	    ExceptionHandler.handle(e);
	}
    }


    public int compareWithDV(DataValue dv) {
	return DVUtil.compareDVs(getDataValue(), dv);
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

    public boolean equalDV(DataValue dv) {
	return equalDV(false, dv);
    }

    public boolean equalDV(boolean inPredicate, DataValue dv) {
	if (dv!=null){
	    if (getDataValue()!=null){
		return DVUtil.equalDVs(getDataValue(), dv);
	    }else{
		return false;
	    }
	}else{
	    return false;
	}
    }

    public boolean isSubClassOf(DataValue... dataValues){
	return isSubClassOf(false, dataValues);
    }

    public boolean isSubClassOf(boolean inPredicate, DataValue... dataValues) {
	    CodePhrase a = getCodePhrase(getDataValue());
	    Set<CodePhrase>  codePhrases = new HashSet<CodePhrase>();
	    for (int i = 0; i < dataValues.length; i++) {
		codePhrases.add(getCodePhrase(dataValues[i]));
	    }
	    if (a!=null && !codePhrases.isEmpty()){
		try {
		    boolean result= CDSTerminologyService.isSubclassOf(a, codePhrases);
		    return result;
		} catch (UnsupportedTerminologyException e) {
		    ExceptionHandler.handle(e);
		    return false;
		} catch (InvalidCodeException e) {
		    ExceptionHandler.handle(e);
		    return false;
		}
	    }else{
		return false;
	    }
    }

    public boolean isNotSubClassOf(DataValue... dataValues){
	//TODO Remove, exceptions should be handled
	CodePhrase a = getCodePhrase(getDataValue());
	Set<CodePhrase>  codePhrases = new HashSet<CodePhrase>();
	for (int i = 0; i < dataValues.length; i++) {
	    codePhrases.add(getCodePhrase(dataValues[i]));    
	}
	if (a!=null && !codePhrases.isEmpty()){
	    try {
		return !CDSTerminologyService.isSubclassOf(a, codePhrases);
	    } catch (UnsupportedTerminologyException e) {
		//TODO Remove, exceptions should be handled
		return false;
	    } catch (InvalidCodeException e) {
		//TODO Remove, exceptions should be handled
		return false;
	    }
	}else{
	    return false;
	}
    }

    private static CodePhrase getCodePhrase(DataValue dv){
	if (dv instanceof DvCodedText){
	    return ((DvCodedText)dv).getDefiningCode();
	}else if (dv instanceof DvOrdinal){
	    return ((DvOrdinal)dv).getSymbol().getDefiningCode();
	}else{
	    return null;
	}
    }

    public boolean nullValueEquals(Object o) {
	if (o instanceof DvCodedText){
	    if (getNullFlavour()!=null){
		return DVUtil.equalDVs(getNullFlavour(), (DataValue)o);
	    }else{
		return false;
	    }
	}else{
	    return false;
	}
    }

    public boolean compatibleComparison(DataValue dv) {
	return DVUtil.compatibleComparison(getDataValue(), dv);
    }

    public ElementInstance clone(){
	return new ElementInstance(id, dataValue, archetypeReference.clone(), containerInstance, nullFlavour);
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