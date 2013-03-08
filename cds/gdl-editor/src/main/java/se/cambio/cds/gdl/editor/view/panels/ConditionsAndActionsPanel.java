package se.cambio.cds.gdl.editor.view.panels;

import java.awt.BorderLayout;

import javax.swing.JPanel;
import javax.swing.JSplitPane;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.LanguageManager;
import se.cambio.cds.gdl.editor.view.applicationobjects.RuleLineDirectory;

public class ConditionsAndActionsPanel extends JPanel {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private JSplitPane condsAndConsqsSplitPane = null;
    private RuleLinesPanel conditionsPanel = null;
    private RuleLinesPanel consequencesPanel = null;
    
    public GDLEditor _controller = null;
    
    public ConditionsAndActionsPanel(GDLEditor controller){
	_controller = controller;
	init();
    }
    
    public void init(){
	this.setLayout(new BorderLayout());
	this.add(getCondsAndConsqsSplitPane());
    }
    
    public JSplitPane getCondsAndConsqsSplitPane(){
	if (condsAndConsqsSplitPane==null){
	    condsAndConsqsSplitPane = new JSplitPane();
	    condsAndConsqsSplitPane.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
	    condsAndConsqsSplitPane.setOneTouchExpandable(true);
	    condsAndConsqsSplitPane.setResizeWeight(0.5);
	    condsAndConsqsSplitPane.setTopComponent(getConditionsPanel());
	    condsAndConsqsSplitPane.setBottomComponent(getConsequencesPanel());
	}
	return condsAndConsqsSplitPane;
    }
    
    private RuleLinesPanel getConditionsPanel(){
	if (conditionsPanel==null){
	    conditionsPanel = 
		    new RuleLinesPanel(
			    _controller, 
			    RuleLineDirectory.getSelectableConditions(),
			    _controller.getConditionRuleLines(),
			    LanguageManager.getMessage("Conditions"));
	}
	return conditionsPanel;
    }
    
    private RuleLinesPanel getConsequencesPanel(){
	if (consequencesPanel==null){
	    consequencesPanel =
		    new RuleLinesPanel(
			    _controller, 
			    RuleLineDirectory.getSelectableActions(),
			    _controller.getActionsRuleLines(),
			    LanguageManager.getMessage("Actions"));
	}
	return consequencesPanel;
    }

}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  1.1 (the 'License'); you may not use this file except in compliance with
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