package se.cambio.cds.gdl.model.readable.rule.lines.elements;

import se.cambio.cds.controller.session.data.ArchetypeReferencesManager;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.OpenEHRLanguageManager;

public abstract class RuleLineElement {
    private String labelTextId = null;
    private String labelDescriptionId = null;
    private RuleLine parentRuleLine = null;

    RuleLineElement(RuleLine ruleLine, String text) {
        this.labelTextId = text;
        labelDescriptionId = text;
        this.parentRuleLine = ruleLine;
    }

    public final String getLabelText() {
        return getLabelText(getLanguage());
    }

    public String getLabelText(String lang) {
        return OpenEHRLanguageManager.getMessageWithLanguage(labelTextId, lang);
    }

    public final String getLabelDescription() {
        return getLabelDescription(getLanguage());
    }

    public String getLabelDescription(String lang) {
        return OpenEHRLanguageManager.getMessageWithLanguage(labelDescriptionId, lang);
    }

    public String getLabelTextHTML(String lang) {
        return getLabelText(lang);
    }

    public RuleLine getParentRuleLine() {
        return parentRuleLine;
    }

    public ArchetypeManager getArchetypeManager() {
        return parentRuleLine.getReadableGuide().getArchetypeManager();
    }

    public ArchetypeReferencesManager getArchetypeReferencesManager() {
        return parentRuleLine.getReadableGuide().getArchetypeReferencesManager();
    }

    protected String getLanguage() {
        return getArchetypeManager().getUserConfigurationManager().getLanguage();
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