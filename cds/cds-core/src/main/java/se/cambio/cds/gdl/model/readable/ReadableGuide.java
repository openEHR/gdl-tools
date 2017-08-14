package se.cambio.cds.gdl.model.readable;

import se.cambio.cds.controller.session.data.ArchetypeReferencesManager;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.gdl.model.readable.rule.ReadableRule;
import se.cambio.cds.gdl.model.readable.rule.RuleLineCollection;
import se.cambio.openehr.controller.session.data.ArchetypeManager;

import java.util.LinkedHashMap;

public class ReadableGuide {
    private LinkedHashMap<String, ReadableRule> renderableRules = null;
    private RuleLineCollection definitionRuleLines = null;
    private RuleLineCollection preconditionRuleLines = null;
    private RuleLineCollection defaultActions = null;
    private TermDefinition termDefinition = null;
    private ArchetypeReferencesManager archetypeReferencesManager;
    private ArchetypeManager archetypeManager;

    public ReadableGuide(
            TermDefinition termDefinition,
            ArchetypeManager archetypeManager,
            ArchetypeReferencesManager archetypeReferencesManager) {
        this.termDefinition = termDefinition;
        this.archetypeReferencesManager = archetypeReferencesManager;
        renderableRules = new LinkedHashMap<>();
        this.archetypeManager = archetypeManager;
    }

    public ArchetypeManager getArchetypeManager() {
        return archetypeManager;
    }

    public ArchetypeReferencesManager getArchetypeReferencesManager() {
        return archetypeReferencesManager;
    }

    public String getLanguage() {
        return termDefinition.getId();
    }

    public LinkedHashMap<String, ReadableRule> getReadableRules() {
        return renderableRules;
    }

    public RuleLineCollection getDefinitionRuleLines() {
        if (definitionRuleLines == null) {
            definitionRuleLines = new RuleLineCollection(this);
        }
        return definitionRuleLines;
    }

    public RuleLineCollection getPreconditionRuleLines() {
        if (preconditionRuleLines == null) {
            preconditionRuleLines = new RuleLineCollection(this);
        }
        return preconditionRuleLines;
    }

    public RuleLineCollection getDefaultActions() {
        if (defaultActions == null) {
            defaultActions = new RuleLineCollection(this);
        }
        return defaultActions;
    }

    public TermDefinition getTermDefinition() {
        return termDefinition;
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