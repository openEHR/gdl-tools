package se.cambio.cds.gdl.model.readable.rule.lines.elements;

import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.openehr.util.OpenEHRLanguageManager;

import java.util.ArrayList;
import java.util.Collection;

public class ExistenceOperatorRuleLineElement extends RuleLineElementWithValue<String> implements SingleSelectionRuleElement<String>{

    private static String HAS_VALUE = "!=null";
    private static String HAS_NO_VALUE = "==null";

    private ArrayList<String> _codes = null;

    public ExistenceOperatorRuleLineElement(RuleLine ruleLine) {
        super(ruleLine, "??");
        _codes = new ArrayList<String>();
        _codes.add(HAS_VALUE);
        _codes.add(HAS_NO_VALUE);
    }

    public String getResolvedName(String item) {
        if (HAS_VALUE.equals(item)){
            return OpenEHRLanguageManager.getMessage("ExistsRLE");
        }else if (HAS_NO_VALUE.equals(item)){
            return OpenEHRLanguageManager.getMessage("DoesNotExistRLE");
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
        if (HAS_VALUE.equals(getValue())){
            return OperatorKind.INEQUAL;
        }else if (HAS_NO_VALUE.equals(getValue())){
            return OperatorKind.EQUALITY;
        }else{
            return null;
        }
    }

    public void setOperator(OperatorKind operatorKind){
        if (OperatorKind.INEQUAL.equals(operatorKind)){
            setValue(HAS_VALUE);
        }else if (OperatorKind.EQUALITY.equals(operatorKind)){
            setValue(HAS_NO_VALUE);
        }
    }

    @Override
    public String toHTMLString() {
        return toString();
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