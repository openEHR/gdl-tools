package se.cambio.cds.gdl.model.readable.rule.lines.elements;

import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRLanguageManager;


public abstract class RuleLineElementWithValue<E> extends RuleLineElement{

    private E value = null;
    private RuleLine parentRuleLine = null;

    public RuleLineElementWithValue(RuleLine ruleLine, String text) {
        super(text);
        parentRuleLine = ruleLine;
    }

    public E getValue() {
        return value;
    }

    public void setValue(E value) {
        this.value = value;
    }

    public String getDescription() {
        return toString();
    }

    public RuleLine getParentRuleLine() {
        return parentRuleLine;
    }

    public String toString(){
        if (value!=null){
            return value.toString();
        }else{
            return getText();
        }
    }

    public String getName(String gtCode){
        if (OpenEHRConst.CURRENT_DATE_TIME_ID.equals(gtCode)){
            return OpenEHRLanguageManager.getMessage("CurrentDateTime");
        }else if (getParentRuleLine()!=null && getParentRuleLine().getTermDefinition()!=null && getParentRuleLine().getTermDefinition().getTerms()!=null){
            Term term = getParentRuleLine().getTermDefinition().getTerms().get(gtCode);
            if (term!=null){
                return term.getText();
            }else{
                return null;
            }
        }else{
            return null;
        }
    }

    public String getDescription(String gtCode){
        if (OpenEHRConst.CURRENT_DATE_TIME_ID.equals(gtCode)){
            return OpenEHRLanguageManager.getMessage("CurrentDateTime");
        }else if (getParentRuleLine()!=null && getParentRuleLine().getTermDefinition()!=null && getParentRuleLine().getTermDefinition().getTerms()!=null){
            Term term = getParentRuleLine().getTermDefinition().getTerms().get(gtCode);
            if (term!=null){
                return term.getDescription();
            }else{
                return null;
            }
        }else{
            return null;
        }
    }

    @Override
    public String toHTMLString() {
        return toString();
    }
}/*
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