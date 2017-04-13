package se.cambio.cds.gdl.model.readable.rule.lines;

import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeReferenceRuleLineDefinitionElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.GTCodeRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.StaticTextRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ArchetypeReferenceRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.DefinitionsRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.GTCodeDefiner;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.InstantiationRuleLine;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.openehr.util.OpenEHRLanguageManager;


public class ArchetypeInstantiationRuleLine extends RuleLine implements ArchetypeReferenceRuleLine, DefinitionsRuleLine, GTCodeDefiner, InstantiationRuleLine {

    private ArchetypeReferenceRuleLineDefinitionElement archetypeReferenceRuleLineDefinitionElement = null;
    private GTCodeRuleLineElement gtCodeRuleLineElement = null;


    public ArchetypeInstantiationRuleLine() {
        super(OpenEHRLanguageManager.getMessage("ArchetypeInstantiation"),
                OpenEHRLanguageManager.getMessage("ArchetypeInstantiationDesc"));
        archetypeReferenceRuleLineDefinitionElement = new ArchetypeReferenceRuleLineDefinitionElement(this);
        gtCodeRuleLineElement = new GTCodeRuleLineElement(this);
        getRuleLineElements().add(new StaticTextRuleLineElement(this,"InstantiateArchetypeRLE"));
        getRuleLineElements().add(archetypeReferenceRuleLineDefinitionElement);
    }

    public String getIdArchetype(){
        return getArchetypeReference().getIdArchetype();
    }

    public ArchetypeReferenceRuleLineDefinitionElement getArchetypeReferenceRuleLineDefinitionElement() {
        return archetypeReferenceRuleLineDefinitionElement;
    }

    @Override
    public ArchetypeReference getArchetypeReference() {
        return getArchetypeReferenceRuleLineDefinitionElement().getValue();
    }

    public void setArchetypeReference(ArchetypeReference ar) {
        getArchetypeReferenceRuleLineDefinitionElement().setValue(ar);
    }

    public String getGTCode() {
        return getGTCodeRuleLineElement().getValue();
    }

    public void setGTCode(String term) {
        getGTCodeRuleLineElement().setValue(term);
    }

    public GTCodeRuleLineElement getGTCodeRuleLineElement() {
        return gtCodeRuleLineElement;
    }

    public String toHTMLString(int level, String lang){
        StringBuilder sb = new StringBuilder();
        sb.append(toHTMLStringSingle(level, lang)).append("<br/>");
        String prefix = "";
        for (RuleLine ruleLine : getChildrenRuleLines().getRuleLines()) {
            sb.append(prefix);
            sb.append(ruleLine.toHTMLString(level+1, lang));
            prefix = "<br/>";
        }
        return sb.toString();
    }

    private String toHTMLStringSingle(int level, String lang){
        return super.toHTMLString(level, lang);
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