package se.cambio.cds.gdl.model.readable.rule.lines;

import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.cds.gdl.model.expression.AssignmentExpression;
import se.cambio.cds.gdl.model.expression.ConstantExpression;
import se.cambio.cds.gdl.model.expression.Variable;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeElementRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.NullValueRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.StaticTextRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ActionRuleLine;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRLanguageManager;


public class SetElementWithNullValueActionRuleLine extends AssignmentExpressionRuleLine implements ActionRuleLine{

    private ArchetypeElementRuleLineElement archetypeElementRuleLineElement = null;
    private NullValueRuleLineElement nullValueRuleLineElement = null;


    public SetElementWithNullValueActionRuleLine() {
	super(OpenEHRLanguageManager.getMessage("SetElementWithNullValue"), 
		OpenEHRLanguageManager.getMessage("SetElementWithNullValueDesc"));
	archetypeElementRuleLineElement = new ArchetypeElementRuleLineElement(this);
	nullValueRuleLineElement = new NullValueRuleLineElement(this);

	getRuleLineElements().add(new StaticTextRuleLineElement(OpenEHRLanguageManager.getMessage("SetElementRLE")));
	getRuleLineElements().add(archetypeElementRuleLineElement);
	getRuleLineElements().add(new StaticTextRuleLineElement(OpenEHRLanguageManager.getMessage("ToRLE")));
	getRuleLineElements().add(nullValueRuleLineElement);
    }

    public ArchetypeElementRuleLineElement getArchetypeElementRuleLineElement(){
	return archetypeElementRuleLineElement;
    }

    public NullValueRuleLineElement getNullValueRuleLineElement(){
	return nullValueRuleLineElement;
    }

    public AssignmentExpression toAssignmentExpression() throws IllegalStateException{
	if (archetypeElementRuleLineElement.getValue()==null){
	    throw new IllegalStateException("No variable set");
	}
	Variable var = new Variable(
		archetypeElementRuleLineElement.getValue().getValue(),
		null, 
		null, 
		OpenEHRConst.NULL_FLAVOR_ATTRIBUTE);
	DvCodedText nullValue = nullValueRuleLineElement.getValue();
	if (nullValue==null){
	    throw new IllegalStateException("No null value set");
	}
	String dataValueStr = nullValue.toString();
	Logger.getLogger(SetElementAttributeActionRuleLine.class).debug("dataValueStr: " + dataValueStr);
	return new AssignmentExpression(
		var, 
		new ConstantExpression(dataValueStr));
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