package se.cambio.cds.gdl.editor.view.panels;
import java.awt.Component;
import java.util.ArrayList;

import javax.swing.JComponent;

import se.cambio.cds.gdl.model.readable.rule.ReadableRule;


public class RuleDropPanel extends DropPanel {
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    private RulesPanel _rulesPanel = null;
    public RuleDropPanel(RulesPanel rulesPanel) {
	super();
	_rulesPanel = rulesPanel;
    }

    public void addDraggableLine(JComponent component, ReadableRule rule){
	DraggablePanel dp = new DraggableRulePanel(component, rule);
	super.add(dp, getGBC());
	getGBC().gridy++;
    }


    public void panelDragged(DraggablePanel panel){
	super.panelDragged(panel);
	ArrayList<ReadableRule> rules = new ArrayList<ReadableRule>();
	for (Component component : getComponents()) {
	    DraggableRulePanel draggablePanel = (DraggableRulePanel)component;
	    rules.add(draggablePanel.getRule());
	}
	_rulesPanel.updateList(rules);
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