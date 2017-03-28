package se.cambio.cds.gdl.editor.view.panels.rulelinecontainers;

import java.awt.BorderLayout;
import java.awt.FlowLayout;

import javax.swing.Box;
import javax.swing.JLabel;
import javax.swing.JPanel;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.applicationobjects.ReadableRuleLineFactory;
import se.cambio.cds.gdl.editor.view.applicationobjects.RuleLineDirectory;
import se.cambio.cds.gdl.editor.view.panels.RuleLinesPanel;
import se.cambio.cds.gdl.model.readable.rule.lines.OrOperatorRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;

public class OrOperatorRuleLinePanel extends RuleLineContainerPanel {

    private OrOperatorRuleLine ruleLine = null;

    private static final long serialVersionUID = 1L;

    public OrOperatorRuleLinePanel(RuleLinesPanel ruleLinesPanel, OrOperatorRuleLine ruleLine, GDLEditor gdlEditor) {
        this.ruleLine = ruleLine;
        this.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));
        JPanel orMainPanel = new JPanel(new BorderLayout(0, 0));
        this.add(orMainPanel);
        JPanel iconPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        JLabel iconLabel = new JLabel("((");
        iconPanel.add(iconLabel);
        iconLabel.setIcon(RuleLineDirectory.getIconForRuleLine(ruleLine));
        iconLabel.addMouseListener(ruleLinesPanel.getSelectableRuleLineDragMouseListener());
        iconLabel.addMouseMotionListener(ruleLinesPanel.getSelectableRuleLineDragMouseListener());
        if (ruleLine.isCommented()) {
            iconLabel.setEnabled(false);
        }
        iconPanel.add(ReadableRuleLineFactory.createCommentButton(ruleLine, ruleLinesPanel));
        iconPanel.add(Box.createHorizontalStrut(2));
        iconPanel.add(ReadableRuleLineFactory.createDeleteButton(ruleLine, ruleLinesPanel, gdlEditor));

        orMainPanel.add(iconPanel, BorderLayout.NORTH);
        JPanel leftPanel = new JPanel(new BorderLayout(0, 0));
        leftPanel.add(Box.createHorizontalStrut(19), BorderLayout.WEST);
        leftPanel.add(new MultipleRuleLinePanel(ruleLinesPanel, ruleLine.getLeftRuleLineBranch(), gdlEditor), BorderLayout.CENTER);
        orMainPanel.add(leftPanel, BorderLayout.CENTER);
        JPanel aux = new JPanel(new BorderLayout(0, 0));
        orMainPanel.add(aux, BorderLayout.SOUTH);
        JPanel orPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        orPanel.add(Box.createHorizontalStrut(19));
        JLabel orLabel = new JLabel(") " + GDLEditorLanguageManager.getMessage("OrRLE") + " (");
        orPanel.add(orLabel);
        if (ruleLine.isCommented()) {
            orLabel.setEnabled(false);
        }
        aux.add(orPanel, BorderLayout.NORTH);
        JPanel rightPanel = new JPanel(new BorderLayout(0, 0));
        rightPanel.add(Box.createHorizontalStrut(19), BorderLayout.WEST);
        rightPanel.add(new MultipleRuleLinePanel(ruleLinesPanel, ruleLine.getRightRuleLineBranch(), gdlEditor), BorderLayout.CENTER);
        aux.add(rightPanel, BorderLayout.CENTER);
        JPanel closurePanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        //closurePanel.add(Box.createHorizontalStrut(19));
        JLabel closureLabel = new JLabel("))");
        closurePanel.add(closureLabel);
        if (ruleLine.isCommented()) {
            closureLabel.setEnabled(false);
        }
        aux.add(closurePanel, BorderLayout.SOUTH);
    }

    public RuleLine getRuleLine() {
        return ruleLine;
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