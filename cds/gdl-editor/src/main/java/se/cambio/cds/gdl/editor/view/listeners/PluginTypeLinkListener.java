package se.cambio.cds.gdl.editor.view.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import se.cambio.cds.gdl.editor.view.labels.JLinkRuleElementLabel;
import se.cambio.cds.gdl.editor.view.panels.RuleLinesPanel;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeElementRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.RuleLineElementWithValue;

public class PluginTypeLinkListener implements ActionListener {

    private RuleLinesPanel _ruleLinesPanel = null;

    public PluginTypeLinkListener(RuleLinesPanel ruleLinesPanel){
	_ruleLinesPanel = ruleLinesPanel;
    }

    public void actionPerformed(ActionEvent e) {
	JLinkRuleElementLabel linkRuleLabel = (JLinkRuleElementLabel)e.getSource();
	RuleLineElementWithValue<?> ruleLineElementWithValue = linkRuleLabel.getRuleLineElementWithValue();
	if (ruleLineElementWithValue!=null){
	    if (JLinkRuleElementLabel.ACTION_RIGHT_CLICK.equals(e.getActionCommand())){
		/*Rename element*/
		if (ruleLineElementWithValue instanceof ArchetypeElementRuleLineElement){
		    ArchetypeElementRuleLineElement archetypeElementRuleLineElement = (ArchetypeElementRuleLineElement)ruleLineElementWithValue;
		    _ruleLinesPanel.getController().editRuleElement(archetypeElementRuleLineElement.getValue());

		}
	    }else{
		/* Edit object*/
		_ruleLinesPanel.getController().editRuleElement(ruleLineElementWithValue);
		_ruleLinesPanel.refresh();
	    }
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