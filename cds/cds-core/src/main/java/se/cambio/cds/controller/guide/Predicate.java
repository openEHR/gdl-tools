package se.cambio.cds.controller.guide;

import java.util.HashSet;
import java.util.Set;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;

import se.cambio.cds.gdl.model.Binding;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.TermBinding;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.util.DVUtil;
import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.openehr.util.ExceptionHandler;

public class Predicate {
    private OperatorKind operatorKind = null;
    private DataValue dataValue = null;

    public Predicate(OperatorKind operatorKind, DataValue dataValue) {
	super();
	this.operatorKind = operatorKind;
	this.dataValue = dataValue;
    }

    public OperatorKind getOperatorKind() {
	return operatorKind;
    }
    public void setOperatorKind(OperatorKind operatorKind) {
	this.operatorKind = operatorKind;
    }
    public DataValue getDataValue() {
	return dataValue;
    }
    public void setDataValue(DataValue dataValue) {
	this.dataValue = dataValue;
    }

    public boolean matches(DataValue dv, String guideId, GuideManager guideManager){
	if (OperatorKind.IS_A.equals(operatorKind)){
	    if (dv instanceof DvCodedText && getDataValue() instanceof DvCodedText){
		CodePhrase elementCodePhrase = ((DvCodedText)dv).getDefiningCode();
		CodePhrase predicateCodePhrase = ((DvCodedText)getDataValue()).getDefiningCode();
		Set<CodePhrase> codePhrases = new HashSet<CodePhrase>();
		if (guideManager!=null){
		    Guide guide = guideManager.getGuide(guideId);
		    if (guide!=null){
			if (guide.getOntology().getTermBindings()!=null){
			    for (String terminologyId : guide.getOntology().getTermBindings().keySet()) {
				TermBinding termBinding = guide.getOntology().getTermBindings().get(terminologyId);
				if (termBinding!=null){
				    Binding binding = termBinding.getBindings().get(predicateCodePhrase.getCodeString());
				    if (binding!=null && binding.getCodes()!=null){
					codePhrases.addAll(binding.getCodes());
				    }
				}
			    }
			}
		    }
		}else{
		    codePhrases.add(predicateCodePhrase);
		}
		if (!codePhrases.isEmpty()){
		    try{
			return OpenEHRSessionManager.getTerminologyFacadeDelegate().isSubclassOf(elementCodePhrase, codePhrases);
		    }catch(Exception e){
			ExceptionHandler.handle(e);
		    }
		}else{
		    return false;
		}
	    }

	}else{
	    if (dataValue!=null){
		return DVUtil.equalDVs(getDataValue(), dv);
	    }else{
		if (dataValue==dv){
		    return true;
		}
	    }
	}
	return false;
    }

    public int hashCode(){
	return operatorKind.hashCode()+(dataValue!=null?dataValue.hashCode():0);
    }

    public String toString(){
	return "OP="+operatorKind+", "+dataValue!=null?dataValue.toString():"NULL_DV";
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