package se.cambio.cds.gdl.model.readable.rule.lines.elements;

import org.apache.log4j.Logger;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.util.ReadableArchetypeReferencesUtil;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.util.OpenEHRLanguageManager;

public class ArchetypeReferenceRuleLineDefinitionElement extends RuleLineElementWithValue<ArchetypeReference> {

    public ArchetypeReferenceRuleLineDefinitionElement(ArchetypeInstantiationRuleLine ruleLine) {
        super(ruleLine, OpenEHRLanguageManager.getMessage("Archetype"));
    }

    public String getDomainId(){
        return getValue().getIdDomain();
    }

    public String getTemplateId(){
        if (getValue()!=null){
            return getValue().getIdTemplate();
        }else{
            return null;
        }
    }

    @Override
    public String getDescription() {
        if (getValue()==null){
            return OpenEHRLanguageManager.getMessage("Archetype");
        }else{
            return ReadableArchetypeReferencesUtil.getHTMLTooltip((ArchetypeInstantiationRuleLine)getParentRuleLine());
        }
    }

    public String toString(){
        if (getValue()!=null){
            String idArchetype = getValue().getIdArchetype();
            ArchetypeDTO archetypeVO = Archetypes.getArchetypeDTO(idArchetype);
            if (archetypeVO!=null){
                return archetypeVO.getName();
            }else{
                Logger.getLogger(this.getClass()).error("Archetype not found! ("+idArchetype+")");
                return null;
            }
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