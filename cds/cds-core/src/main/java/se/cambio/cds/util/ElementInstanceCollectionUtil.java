package se.cambio.cds.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;

import se.cambio.cds.gdl.model.Binding;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.TermBinding;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.model.facade.execution.vo.GeneratedArchetypeReference;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.openehr.util.ExceptionHandler;

public class ElementInstanceCollectionUtil {


    public static boolean isEmpty(ArchetypeReference ar){
	for (String idElement : ar.getElementInstancesMap().keySet()) {
	    ElementInstance ei = ar.getElementInstancesMap().get(idElement);
	    if (ei.getDataValue()!=null){
		return false;
	    }
	}
	return true;
    }

    public static ArchetypeReference getEmptyArchetypeReference(Set<ArchetypeReference> archetypeReferences){
	for (ArchetypeReference archetypeReference : archetypeReferences) {
	    if (isEmpty(archetypeReference)){
		return archetypeReference;
	    }
	}
	return null;
    }

    public static boolean containsAll(ArchetypeReference ar1, ArchetypeReference ar2){
	return ar1.getElementInstancesMap().keySet().containsAll(ar2.getElementInstancesMap().keySet());
    }

    /**
     * Looks if the second reference matches the first one, if empty references are found, they will be copied into the second one (only if the rest is matched).
     * @param ar1
     * @param ar2
     * @param guide
     * @return true if ar1 matches ar2
     */
    public static boolean matchAndFill(GeneratedArchetypeReference ar1, ArchetypeReference ar2, Guide guide){
	Collection<ElementInstance> emptyElementInstances = new ArrayList<ElementInstance>();
	for (String  idElement : ar1.getElementInstancesMap().keySet()) {
	    ElementInstance ei1 = ar1.getElementInstancesMap().get(idElement);
	    ElementInstance ei2 = ar2.getElementInstancesMap().get(idElement);
	    if (ei1 instanceof PredicateGeneratedElementInstance){
		if (ei2!=null){
		    PredicateGeneratedElementInstance pgei = ((PredicateGeneratedElementInstance)ei1);
		    if (!matches(ei1.getDataValue(), ei2.getDataValue(), pgei.getOperatorKind(), guide)){
			return false;
		    }
		}else{
		    return false;
		}
	    }else{
		if (ei2==null){
		    emptyElementInstances.add(ei1.clone());
		}
	    }
	}
	//Set AR to empty elementInstances found
	for (ElementInstance elementInstance : emptyElementInstances) {
	    elementInstance.setArchetypeReference(ar2);
	}
	return true;
    }

    public static boolean matches(GeneratedArchetypeReference ar1, ArchetypeReference ar2, Guide guide){
	for (String  idElement : ar1.getElementInstancesMap().keySet()) {
	    ElementInstance ei1 = ar1.getElementInstancesMap().get(idElement);
	    ElementInstance ei2 = ar2.getElementInstancesMap().get(idElement);
	    if (ei1 instanceof PredicateGeneratedElementInstance){
		if (ei2!=null){
		    PredicateGeneratedElementInstance pgei = ((PredicateGeneratedElementInstance)ei1);
		    if (!matches(ei1.getDataValue(), ei2.getDataValue(), pgei.getOperatorKind(), guide)){
			return false;
		    }
		}else{
		    return false;
		}
	    }
	}
	return true;
    }

    public static boolean matches(
	    DataValue dv1, 
	    DataValue dv2, 
	    OperatorKind operatorKind, 
	    Guide guide){
	//TODO Only == and IS_A operators are supported
	if (OperatorKind.IS_A.equals(operatorKind)){
	    if (dv1 instanceof DvCodedText && dv2 instanceof DvCodedText){
		CodePhrase elementCodePhrase = ((DvCodedText)dv2).getDefiningCode();
		CodePhrase predicateCodePhrase = ((DvCodedText)dv1).getDefiningCode();
		Set<CodePhrase> codePhrases = new HashSet<CodePhrase>();
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
	    return DVUtil.equalDVs(dv1, dv2);
	}
	return false;
    }

    public static DataValue resolvePredicate(DataValue dv, OperatorKind op, Guide guide){
	if (OperatorKind.IS_A.equals(op)){
	    if (dv instanceof DvCodedText){
		DvCodedText dvCT = (DvCodedText)dv;
		if (guide!=null && guide.getOntology().getTermBindings()!=null){
		    for (String terminologyId : guide.getOntology().getTermBindings().keySet()) {
			TermBinding termBinding = guide.getOntology().getTermBindings().get(terminologyId);
			if (termBinding!=null){
			    Binding binding = termBinding.getBindings().get(dvCT.getDefiningCode().getCodeString());
			    if (binding!=null && binding.getCodes()!=null && !binding.getCodes().isEmpty()){
				CodePhrase cf = binding.getCodes().get(0);
				return new DvCodedText(dvCT.getValue(),cf);
			    }
			}
		    }
		}
		//If reaches here, no terminology was found (problem)
		Logger.getLogger(ElementInstanceCollectionUtil.class).warn("Not terminology binding for '"+dv+"' was found.");
		return null;
	    }else{
		Logger.getLogger(ElementInstanceCollectionUtil.class).warn("Not a coded text '"+dv+"'");
		return null;
	    }
	}else{
	    return dv;
	}
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