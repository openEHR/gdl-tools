package se.cambio.cds.gdl.editor.view.panels;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.view.swing.panel.interfaces.RefreshablePanel;

import javax.swing.*;
import java.awt.*;

public class ConditionsAndActionsPanel extends JPanel implements RefreshablePanel {

    private static final long serialVersionUID = 1L;
    private JSplitPane conditionsAndActionsSplitPane = null;
    private RuleLinesPanel conditionsPanel = null;
    private RuleLinesPanel consequencesPanel = null;

    public GDLEditor _controller = null;

    ConditionsAndActionsPanel(GDLEditor controller) {
        _controller = controller;
        init();
    }

    public void init() {
        this.setLayout(new BorderLayout());
        this.add(getConditionsAndActionsSplitPane());
    }

    private JSplitPane getConditionsAndActionsSplitPane() {
        if (conditionsAndActionsSplitPane == null) {
            conditionsAndActionsSplitPane = new JSplitPane();
            conditionsAndActionsSplitPane.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
            conditionsAndActionsSplitPane.setOneTouchExpandable(true);
            conditionsAndActionsSplitPane.setResizeWeight(0.5);
            conditionsAndActionsSplitPane.setTopComponent(getConditionsPanel());
            conditionsAndActionsSplitPane.setBottomComponent(getConsequencesPanel());
        }
        return conditionsAndActionsSplitPane;
    }

    private RuleLinesPanel getConditionsPanel() {
        if (conditionsPanel == null) {
            conditionsPanel = new ConditionRuleLinesPanel(_controller);
        }
        return conditionsPanel;
    }

    private RuleLinesPanel getConsequencesPanel() {
        if (consequencesPanel == null) {
            consequencesPanel = new ActionRuleLinesPanel(_controller);
        }
        return consequencesPanel;
    }

    @Override
    public void refresh() {
        getConditionsPanel().refresh();
        getConsequencesPanel().refresh();
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