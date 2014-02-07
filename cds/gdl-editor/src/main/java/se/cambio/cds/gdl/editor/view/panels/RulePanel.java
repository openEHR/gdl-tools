package se.cambio.cds.gdl.editor.view.panels;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.panels.interfaces.RefreshablePanel;

import javax.swing.*;
import java.awt.*;

public class RulePanel extends JPanel implements RefreshablePanel{

    private static final long serialVersionUID = 1L;

    private JTabbedPane ruleEditorTabPane = null;
    private GDLEditor _controller = null;

    //private JPanel configurationPanel;

    private ConditionsAndActionsPanel conditionsAndActionsPanel;

    public RulePanel(GDLEditor controller){
        _controller=controller;
        initialize();
    }

    private void initialize(){
        this.setLayout(new BorderLayout());
        this.add(getRuleEditorTabPane());
    }

    private JTabbedPane getRuleEditorTabPane(){
        if (ruleEditorTabPane == null){
            ruleEditorTabPane = new JTabbedPane();
	    /*TODO
	    ruleEditorTabPane.addTab(
		    LanguageManager.getMessage("Configuration"),
		    ImageUtil.RULE_ICON, 
		    getConfigurationPanel());*/
            ruleEditorTabPane.addTab(
                    GDLEditorLanguageManager.getMessage("ConditionsAndActions"),
                    GDLEditorImageUtil.RULE_ICON,
                    getConditionsAndActionsPanel());
		    /*
	    SwingUtilities.invokeLater(new Runnable() {
		public void run() {
		    ruleEditorTabPane.setSelectedIndex(1);
		}
	    });*/

        }
        return ruleEditorTabPane;
    }
    /*
        private JPanel getConfigurationPanel(){
        if (configurationPanel==null){
            configurationPanel = new JPanel();
        }
        return configurationPanel;
        }
    */
    private ConditionsAndActionsPanel getConditionsAndActionsPanel(){
        if (conditionsAndActionsPanel==null){
            conditionsAndActionsPanel = new ConditionsAndActionsPanel(_controller);
        }
        return conditionsAndActionsPanel;
    }

    @Override
    public void refresh() {
        getConditionsAndActionsPanel().refresh();
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