package se.cambio.cds.gdl.model.readable.rule.lines.elements;

import se.cambio.cds.controller.session.data.ArchetypeReferences;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ArchetypeReferenceRuleLine;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRLanguageManager;

public class ArchetypeElementRuleLineDefinitionElement extends RuleLineElementWithValue<ArchetypeElementVO> {

    public ArchetypeElementRuleLineDefinitionElement(RuleLine ruleLine) {
        super(ruleLine, OpenEHRLanguageManager.getMessage("Element"));
    }

    public ArchetypeReference getArchetypeReference() {
        if (getParentRuleLine() instanceof ArchetypeReferenceRuleLine){
            return ((ArchetypeReferenceRuleLine)getParentRuleLine()).getArchetypeReference();
        }else{
            return null;
        }
    }

    @Override
    public String getDescription() {
        if (getValue()==null){
            return OpenEHRLanguageManager.getMessage("Element");
        }else{
            return ArchetypeReferences.getHTMLTooltip(getValue(), getArchetypeReference());
        }
    }

    public String getDomainId(){
        return getArchetypeReference().getIdDomain();
    }

    public String toString(){
        if (getValue()!=null){
            return getValue().getName();
        }else{
            return getText();
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