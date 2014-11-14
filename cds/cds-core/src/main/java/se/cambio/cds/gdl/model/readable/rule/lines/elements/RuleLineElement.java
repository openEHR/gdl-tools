package se.cambio.cds.gdl.model.readable.rule.lines.elements;

import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.openehr.controller.session.data.ArchetypeManager;

public abstract class RuleLineElement {
    private String text = null;
    private String description = null;
    private RuleLine parentRuleLine = null;

    public RuleLineElement(RuleLine ruleLine, String text) {
        this.text = text;
        this.description = text;
        this.parentRuleLine = ruleLine;
    }

    public String getText() {
        return text;
    }

    public String toString(){
        return text;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public abstract String toHTMLString(String lang);

    public RuleLine getParentRuleLine() {
        return parentRuleLine;
    }

    public ArchetypeManager getArchetypeManager(){
        return parentRuleLine.getReadableGuide().getArchetypeManager();
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