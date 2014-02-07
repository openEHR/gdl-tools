package se.cambio.cds.gdl.editor.view.panels;

import javax.swing.JLabel;

import se.cambio.cds.gdl.editor.view.applicationobjects.RuleLineDirectory;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;

public class DraggableSelectableRuleLinePanel extends JLabel {
    private static final long serialVersionUID = 1L;
    private RuleLine _ruleLine = null;
    
    public DraggableSelectableRuleLinePanel(RuleLine ruleLine) {
	super();
	this.setText(ruleLine.getName());
	this.setIcon(RuleLineDirectory.getIconForRuleLine(ruleLine));
	this.setToolTipText(ruleLine.getDescription());
	_ruleLine = ruleLine;
    }
    
    public RuleLine getRuleLine(){
	return _ruleLine;
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