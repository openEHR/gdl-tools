package se.cambio.cds.gdl.model.readable.rule.lines.elements;

import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.openehr.util.OpenEHRLanguageManager;

import java.util.ArrayList;
import java.util.Collection;

public class IsAOperatorRuleLineElement extends RuleLineElementWithValue<String> implements SingleSelectionRuleElement<String>{

    private static String IS_A_VALUE = "is_a";
    private static String IS_NOT_A_VALUE = "is_not_a";

    private ArrayList<String> _codes = null;

    public IsAOperatorRuleLineElement(RuleLine ruleLine) {
	super(ruleLine, "??");
	_codes = new ArrayList<String>();
	_codes.add(IS_A_VALUE);
	_codes.add(IS_NOT_A_VALUE);
    }

    public String getResolvedName(String item) {
	if (IS_A_VALUE.equals(item)){
	    return OpenEHRLanguageManager.getMessage("IsARLE");
	}else if (IS_NOT_A_VALUE.equals(item)){
	    return OpenEHRLanguageManager.getMessage("IsNotARLE");
	}else{
	    return null;
	}
    }

    public String getResolvedDescription(String item) {
	return getResolvedName(item);
    }

    public String toString(){
	if (getValue()!=null){
	    return getResolvedName(getValue());
	}else{
	    return super.getText();
	}
    }

    public Collection<String> getItems() {
	return _codes;
    }

    public OperatorKind getOperator() {
	if (IS_A_VALUE.equals(getValue())){
	    return OperatorKind.IS_A;
	}else if (IS_NOT_A_VALUE.equals(getValue())){
	    return OperatorKind.IS_NOT_A;
	}else{
	    return null;
	}
    }

    public void setOperator(OperatorKind operatorKind){
	if (OperatorKind.IS_A.equals(operatorKind)){
	    setValue(IS_A_VALUE);
	}else if (OperatorKind.IS_NOT_A.equals(operatorKind)){
	    setValue(IS_NOT_A_VALUE);
	}
    }
    
    @Override
    public String toHTMLString() {
	return "<font color='#c29700'>"+toString()+"</font>";
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