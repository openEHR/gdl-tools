package se.cambio.cds.gdl.model.readable.rule.lines;

import se.cambio.cds.gdl.model.expression.AssignmentExpression;
import se.cambio.cds.gdl.model.expression.Variable;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeElementRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.StaticTextRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ActionRuleLine;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRLanguageManager;


public class SetElementWithElementActionRuleLine extends AssignmentExpressionRuleLine implements ActionRuleLine{

    private ArchetypeElementRuleLineElement archetypeElementRuleLineElement = null;
    private ArchetypeElementRuleLineElement archetypeElementRuleLineElement2 = null;
    

    public SetElementWithElementActionRuleLine() {
	super(OpenEHRLanguageManager.getMessage("SetElementWithElement"), 
		OpenEHRLanguageManager.getMessage("SetElementWithElementDesc"));
	archetypeElementRuleLineElement = new ArchetypeElementRuleLineElement(this);
	archetypeElementRuleLineElement2 = new ArchetypeElementRuleLineElement(this);
	
	getRuleLineElements().add(new StaticTextRuleLineElement(OpenEHRLanguageManager.getMessage("SetElementRLE")));
	getRuleLineElements().add(archetypeElementRuleLineElement);
	getRuleLineElements().add(new StaticTextRuleLineElement(OpenEHRLanguageManager.getMessage("ToRLE")));
	getRuleLineElements().add(archetypeElementRuleLineElement2);
    }
    
    public AssignmentExpression toAssignmentExpression() throws IllegalStateException{
	ArchetypeElementVO archetypeElementVO = getArchetypeElementRuleLineElement().getArchetypeElementVO();
	if (archetypeElementVO==null){
	    throw new IllegalStateException("No variable set");
	}
	Variable var = new Variable(
		archetypeElementRuleLineElement.getValue().getValue(),
		null, archetypeElementVO.getName());
	ArchetypeElementVO archetypeElementVO2 = getSecondArchetypeElementRuleLineElement().getArchetypeElementVO();
	if (archetypeElementVO2==null){
	    throw new IllegalStateException("No variable to assign set");
	}
	Variable varAux = new Variable(
		archetypeElementRuleLineElement2.getValue().getValue(),
		null, archetypeElementVO2.getName());
	return new AssignmentExpression(
		    var, 
		    varAux);//TODO
    }
    public ArchetypeElementRuleLineElement getArchetypeElementRuleLineElement(){
	return archetypeElementRuleLineElement;
    }

    public ArchetypeElementRuleLineElement getSecondArchetypeElementRuleLineElement() {
        return archetypeElementRuleLineElement2;
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