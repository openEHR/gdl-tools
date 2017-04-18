package se.cambio.cds.gdl.editor.view.panels.rulelinecontainers;

import java.awt.BorderLayout;
import java.awt.FlowLayout;

import javax.swing.JLabel;
import javax.swing.JPanel;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.view.panels.RuleLinesPanel;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;

public class OperatorRuleLineContainer extends MultipleRuleLineContainerWithHeader {

    private static final long serialVersionUID = 1L;

    public OperatorRuleLineContainer(RuleLinesPanel ruleLinesPanel, RuleLine ruleLine, GDLEditor gdlEditor) {
        super(ruleLinesPanel, ruleLine, gdlEditor);
        init();
    }

    private void init() {
        JPanel closurePanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        JLabel closureLabel = new JLabel(")");
        if (getRuleLine().isCommented()) {
            closureLabel.setEnabled(false);
        }
        closurePanel.add(closureLabel);
        getMainPanel().add(closurePanel, BorderLayout.SOUTH);
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