package se.cambio.cds.gdl.model.readable.rule.lines.elements;

import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.gdl.model.readable.util.ReadableArchetypeReferencesUtil;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.openehr.util.OpenEHRLanguageManager;

public class CDSEntryRuleLineElement extends RuleLineElementWithValue<GTCodeRuleLineElement> {

    public CDSEntryRuleLineElement(RuleLine ruleLine) {
        super(ruleLine, "Entry");
    }

    public ArchetypeReference getArchetypeReference() {
        ArchetypeInstantiationRuleLine airl = getArchetypeInstantiationRuleLine();
        if (airl != null) {
            return airl.getArchetypeReference();
        } else {
            return null;
        }
    }

    private ArchetypeInstantiationRuleLine getArchetypeInstantiationRuleLine() {
        if (getValue() != null && getValue().getParentRuleLine() instanceof ArchetypeInstantiationRuleLine) {
            return ((ArchetypeInstantiationRuleLine) getValue().getParentRuleLine());
        } else {
            return null;
        }
    }

    @Override
    public String getLabelDescription(String lang) {
        if (getArchetypeInstantiationRuleLine() == null) {
            return OpenEHRLanguageManager.getMessageWithLanguage("Entry", lang);
        } else {
            return ReadableArchetypeReferencesUtil.getHTMLTooltip(getArchetypeInstantiationRuleLine());
        }
    }

    @Override
    public String getLabelText(String lang) {
        if (getArchetypeInstantiationRuleLine() != null) {
            return getArchetypeInstantiationRuleLine().getIdArchetype();
        } else {
            return super.getLabelText(lang);
        }
    }

    @Override
    public String getLabelTextHTML(String lang) {
        return "<font color='#4f81bd'><b>" + getLabelText(lang) + "</b></font>";
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