package se.cambio.cds.gdl.model.readable.rule.lines.elements;

import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.openehr.util.OpenEHRLanguageManager;

public class ArchetypeElementAttributeRuleLineElement extends RuleLineElementWithValue<ArchetypeElementRuleLineElement> {

    private String _attribute = null;

    public ArchetypeElementAttributeRuleLineElement(RuleLine ruleLine) {
        super(ruleLine, OpenEHRLanguageManager.getMessage("ElementAttribute"));
    }

    public ArchetypeReference getArchetypeReference() {
        return getValue().getArchetypeReference();
    }

    @Override
    public String getDescription() {
        if (getValue()!=null){
            return getValue().getDescription();
        }else{
            return getText();
        }
    }

    public String getDomainId(){
        return getArchetypeReference().getIdDomain();
    }

    public String toString(){
        if (getValue()!=null && getAttribute()!=null){
            String name = getName(getValue().getValue().getValue());
            return "\"<b>"+name+"</b><font size=2><sub>"+getAttribute().toUpperCase()+"</sub></font>\"";
        }else{
            return getText();
        }
    }

    @Override
    public String toHTMLString() {
        return "<font color='#4f81bd'><b>"+toString()+"</b></font>";
    }

    public String getAttribute() {
        return _attribute;
    }

    public void setAttribute(String attribute) {
        this._attribute = attribute;
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