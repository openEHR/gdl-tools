package se.cambio.cds.gdl.model.readable.rule.lines.elements;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.DvOrdinal;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.util.DVDefSerializer;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRLanguageManager;

public class DataValueRuleLineElement extends RuleLineElementWithValue<DataValue> {

    public DataValueRuleLineElement(RuleLine ruleLine) {
	super(ruleLine, OpenEHRLanguageManager.getMessage("DataValue"));
    }

    @Override
    public String getDescription() {
	if (getValue()!=null){
	    if (getValue() instanceof DvCodedText){
		CodePhrase cp = ((DvCodedText)getValue()).getDefiningCode();
		Term term = getTerm(cp);
		if (term!=null){
		    return term.getDescription();   
		}else{
		    return ((DvCodedText)getValue()).getValue();
		}
	    }else if (getValue() instanceof DvOrdinal){
		CodePhrase cp = ((DvOrdinal)getValue()).getSymbol().getDefiningCode();
		Term term = getTerm(cp);
		if (term!=null){
		    return term.getDescription();   
		}else{
		    return ((DvOrdinal)getValue()).getSymbol().getValue();
		}
	    }
	}
	return OpenEHRLanguageManager.getMessage("DataValue"); //Default
    }

    public String toString(){
	if (getValue()!=null){
	    return DVDefSerializer.getReadableValue(getValue(), getParentRuleLine().getTermDefinition());
	}else{
	    return getText();
	}
    }

    private Term getTerm(CodePhrase cp){
	if (cp.getTerminologyId().getValue().equals(OpenEHRConst.LOCAL)){
	    TermDefinition termDefinition =  getParentRuleLine().getTermDefinition();
	    if (termDefinition!=null){
		return termDefinition.getTerms().get(cp.getCodeString());
	    }
	}
	return null;
    }

    @Override
    public String toHTMLString() {
	return "<font color='#00803a'><b>"+toString()+"</b></font>";
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