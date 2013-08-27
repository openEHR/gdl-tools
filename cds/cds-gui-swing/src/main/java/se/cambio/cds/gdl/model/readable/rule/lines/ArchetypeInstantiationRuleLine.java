package se.cambio.cds.gdl.model.readable.rule.lines;

import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeReferenceRuleLineDefinitionElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.StaticTextRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ArchetypeReferenceRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.DefinitionsRuleLine;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.openehr.util.OpenEHRLanguageManager;


public class ArchetypeInstantiationRuleLine extends RuleLine implements ArchetypeReferenceRuleLine,DefinitionsRuleLine{

    private ArchetypeReferenceRuleLineDefinitionElement archetypeReferenceRuleLineDefinitionElement = null;
    //private GTCodeRuleLineElement gtCodeRuleLineElement = null;


    public ArchetypeInstantiationRuleLine() {
	super(OpenEHRLanguageManager.getMessage("ArchetypeInstantiation"), 
		OpenEHRLanguageManager.getMessage("ArchetypeInstantiationDesc"));
	archetypeReferenceRuleLineDefinitionElement = new ArchetypeReferenceRuleLineDefinitionElement(this);
	getRuleLineElements().add(new StaticTextRuleLineElement(OpenEHRLanguageManager.getMessage("InstantiateArchetypeRLE")));
	getRuleLineElements().add(archetypeReferenceRuleLineDefinitionElement);
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