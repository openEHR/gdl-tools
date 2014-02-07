package se.cambio.cds.gdl.model.readable.rule.lines.elements;

import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeElementInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.gdl.model.readable.util.ReadableArchetypeElementsUtil;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRLanguageManager;

public class ArchetypeElementRuleLineElement extends RuleLineElementWithValue<GTCodeRuleLineElement> {

    public ArchetypeElementRuleLineElement(RuleLine ruleLine) {
        super(ruleLine, OpenEHRLanguageManager.getMessage("Element"));
    }

    public ArchetypeReference getArchetypeReference() {
        ArchetypeElementInstantiationRuleLine aeirl = getArchetypeElementInstantiationRuleLine();
        if (aeirl!=null){
            return aeirl.getArchetypeReference();
        }else{
            return null;
        }
    }

    public ArchetypeElementVO getArchetypeElementVO(){
        ArchetypeElementInstantiationRuleLine aeirl = getArchetypeElementInstantiationRuleLine();
        if (aeirl!=null){
            return aeirl.getArchetypeElement();
        }else{
            return null;
        }
    }

    @Override
    public String getDescription() {
        if (getValue()!=null && getValue().getValue()!=null){
            if (getArchetypeElementVO()!=null && getArchetypeElementVO().getIdArchetype()!=null){
                return ReadableArchetypeElementsUtil.getHTMLTooltip(getArchetypeElementInstantiationRuleLine());
            }else{
                return getText();
            }
        }else{
            return getText(); //currentDateTime
        }
    }

    private ArchetypeElementInstantiationRuleLine getArchetypeElementInstantiationRuleLine() {
        if (getValue()!=null && getValue().getParentRuleLine() instanceof ArchetypeElementInstantiationRuleLine){
            return ((ArchetypeElementInstantiationRuleLine)getValue().getParentRuleLine());
        }else{
            return null;
        }
    }

    public String getDomainId(){
        ArchetypeReference ar = getArchetypeReference();
        if (ar!=null){
            return ar.getIdDomain();
        }else {
            return null;
        }
    }

    public String toString(){
        if (getValue()!=null){
            return getValue().toString();
        }else{
            return getText();
        }
    }

    @Override
    public String toHTMLString() {
        return "<font color='#4f81bd'><b>"+toString()+"</b></font>";
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