package se.cambio.cds.gdl.model.readable.rule.lines;

import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeElementRuleLineDefinitionElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.GTCodeRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.StaticTextRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ArchetypeElementRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.DefinitionsRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.GTCodeDefiner;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRLanguageManager;


public class ArchetypeElementInstantiationRuleLine extends RuleLine implements ArchetypeElementRuleLine, DefinitionsRuleLine, GTCodeDefiner{

    private ArchetypeElementRuleLineDefinitionElement _archetypeElementRuleLineDefinitionElement = null;
    private GTCodeRuleLineElement _gtCodeRuleLineElement = null;


    public ArchetypeElementInstantiationRuleLine(
            ArchetypeInstantiationRuleLine archetypeInstantiationRuleLine) {
        super(OpenEHRLanguageManager.getMessage("ArchetypeElementInstantiation"),
                OpenEHRLanguageManager.getMessage("ArchetypeElementInstantiationDesc"));
        if (archetypeInstantiationRuleLine!=null){
            archetypeInstantiationRuleLine.addChildRuleLine(this);
        }
        _archetypeElementRuleLineDefinitionElement = new ArchetypeElementRuleLineDefinitionElement(this);
        _gtCodeRuleLineElement = new GTCodeRuleLineElement(this);

        getRuleLineElements().add(new StaticTextRuleLineElement(OpenEHRLanguageManager.getMessage("InstantiateElementRLE")));
        getRuleLineElements().add(_archetypeElementRuleLineDefinitionElement);
        getRuleLineElements().add(new StaticTextRuleLineElement(OpenEHRLanguageManager.getMessage("asRLE")));
        getRuleLineElements().add(_gtCodeRuleLineElement);
    }

    public ArchetypeReference getArchetypeReference(){
        ArchetypeInstantiationRuleLine airl = getArchetypeInstantiationRuleLine();
        if (airl!=null){
            return airl.getArchetypeReferenceRuleLineDefinitionElement().getValue();
        }else{
            return null;
        }
    }

    public ArchetypeElementRuleLineDefinitionElement getArchetypeElementRuleLineDefinitionElement() {
        return _archetypeElementRuleLineDefinitionElement;
    }

    public GTCodeRuleLineElement getGTCodeRuleLineElement() {
        return _gtCodeRuleLineElement;
    }

    public ArchetypeInstantiationRuleLine getArchetypeInstantiationRuleLine() {
        return (ArchetypeInstantiationRuleLine)getParentRuleLine();
    }

    public String getGTCode() {
        return getGTCodeRuleLineElement().getValue();
    }

    public void setGTCode(String term) {
        getGTCodeRuleLineElement().setValue(term);
    }

    public void setArchetypeElementVO(ArchetypeElementVO archetypeElementVO){
        getArchetypeElementRuleLineDefinitionElement().setValue(archetypeElementVO);
    }

    @Override
    public ArchetypeElementVO getArchetypeElement() {
        return getArchetypeElementRuleLineDefinitionElement().getValue();
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