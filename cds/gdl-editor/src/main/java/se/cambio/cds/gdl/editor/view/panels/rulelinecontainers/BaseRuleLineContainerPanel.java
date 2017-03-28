package se.cambio.cds.gdl.editor.view.panels.rulelinecontainers;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.view.applicationobjects.ReadableRuleLineFactory;
import se.cambio.cds.gdl.editor.view.panels.RuleLinesPanel;
import se.cambio.cds.gdl.model.readable.rule.RuleLineCollection;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;

import javax.swing.*;
import java.awt.*;

public class BaseRuleLineContainerPanel extends RuleLineContainerPanel {

    private JPanel ruleLineListPanel = null;

    private static final long serialVersionUID = 1L;

    private RuleLinesPanel ruleLinesPanel = null;
    private RuleLineCollection ruleLines = null;
    private GDLEditor gdlEditor;

    public BaseRuleLineContainerPanel(RuleLinesPanel ruleLinesPanel, RuleLineCollection ruleLines, GDLEditor gdlEditor) {
        this.ruleLinesPanel = ruleLinesPanel;
        this.ruleLines = ruleLines;
        this.gdlEditor = gdlEditor;
        init();
    }

    private void init() {
        this.setLayout(new BorderLayout());
        this.add(getRuleLineListPanel(), BorderLayout.NORTH);
        for (RuleLine ruleLine : ruleLines.getRuleLines()) {
            JPanel panel = ReadableRuleLineFactory.createRuleLineContainer(ruleLinesPanel, ruleLine, gdlEditor);
            getRuleLineListPanel().add(panel);
        }
    }

    private JPanel getRuleLineListPanel() {
        if (ruleLineListPanel == null) {
            ruleLineListPanel = new JPanel();
            ruleLineListPanel.setLayout(new BoxLayout(ruleLineListPanel, BoxLayout.Y_AXIS));
        }
        return ruleLineListPanel;
    }

    public RuleLine getRuleLine() {
        return null;
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