package se.cambio.cds.gdl.editor.view.panels;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.dialog.DialogNameInsert;
import se.cambio.cds.gdl.model.readable.rule.ReadableRule;
import se.cambio.cds.view.swing.panel.interfaces.RefreshablePanel;
import se.cambio.openehr.view.util.JLinkLabel;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.ArrayList;

public class RulesPanel extends JPanel implements RefreshablePanel {

    private static final long serialVersionUID = 1249517640199647302L;
    private JScrollPane ruleListJScrollPane;
    private JPanel ruleListPanel;
    private RuleDropPanel ruleListDropPanel;
    private GDLEditor controller;

    RulesPanel(GDLEditor controller) {
        this.controller = controller;
        init();
    }

    private void init() {
        this.setLayout(new BorderLayout());
        this.add(getRuleListJScrollPane(), BorderLayout.CENTER);
        refresh();
    }

    private JPanel getRuleListPanel() {
        if (ruleListPanel == null) {
            ruleListPanel = new JPanel(new BorderLayout());
        }
        return ruleListPanel;
    }

    void addRule() {
        ReadableRule rule = controller.createNewRule();
        if (rule != null) {
            refresh();
            controller.ruleEdit(rule);
        }
    }

    public void refresh() {
        getRuleListDropPanel().removeAll();
        getRuleListPanel().removeAll();
        if (controller.getRenderableRules().isEmpty()) {
            ruleListPanel.setBorder(new EmptyBorder(10, 10, 10, 10));
            ruleListPanel.add(new JLabel(GDLEditorLanguageManager.getMessage("NoRulesYetUseAddRuleButtonMsg")), BorderLayout.NORTH);
        } else {
            ruleListPanel.setBorder(null);
            ruleListPanel.add(getRuleListDropPanel(), BorderLayout.NORTH);
            for (ReadableRule rule : controller.getRenderableRules().values()) {
                getRuleListDropPanel().addDraggableLine(createRulePanel(rule), rule);
            }
        }
        this.repaint();
        this.validate();
    }

    void updateList(ArrayList<ReadableRule> rules) {
        controller.getRenderableRules().clear();
        for (ReadableRule readableRule : rules) {
            controller.getRenderableRules().put(readableRule.getGTCode(), readableRule);
        }
        refresh();
    }

    private RuleDropPanel getRuleListDropPanel() {
        if (ruleListDropPanel == null) {
            ruleListDropPanel = new RuleDropPanel(this);
        }
        return ruleListDropPanel;
    }

    private JPanel createRulePanel(ReadableRule rule) {
        JPanel rulePanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        JLinkLabel linkLabel = new JLinkLabel();
        linkLabel.setText(controller.getGTName(rule.getGTCode()));
        linkLabel.setIcon(GDLEditorImageUtil.RULE_ICON);
        linkLabel.addActionListener(new LoadRuleAction(rule));
        rulePanel.add(linkLabel);
        linkLabel.setCommented(rule.isCommented());
        rulePanel.add(Box.createHorizontalStrut(5));
        rulePanel.add(createEditNameButton(rule));
        rulePanel.add(Box.createHorizontalStrut(5));
        rulePanel.add(createCommentButton(rule));
        rulePanel.add(Box.createHorizontalStrut(5));
        rulePanel.add(createDeleteButton(rule));
        return rulePanel;
    }

    private JButton createCommentButton(ReadableRule rule) {
        JButton button = createGenericButton();
        button.setAction(new ChangeCommentAction(rule));
        if (rule.isCommented()) {
            button.setIcon(GDLEditorImageUtil.UNACCEPT_ICON);
        } else {
            button.setIcon(GDLEditorImageUtil.ACCEPT_ICON);
        }
        button.setToolTipText(GDLEditorLanguageManager.getMessage("SetActiveInactive"));
        return button;
    }

    private class ChangeCommentAction extends AbstractAction {
        private static final long serialVersionUID = 1L;
        private ReadableRule _rule = null;

        ChangeCommentAction(ReadableRule rule) {
            _rule = rule;
        }

        public void actionPerformed(ActionEvent e) {
            controller.changeCommentRule(_rule, !_rule.isCommented());
            refresh();
        }
    }

    private JButton createDeleteButton(ReadableRule rule) {
        JButton button = createGenericButton();
        button.setAction(new DeleteRuleAction(rule));
        button.setIcon(GDLEditorImageUtil.DELETE_ICON);
        button.setToolTipText(GDLEditorLanguageManager.getMessage("DeleteRule"));
        return button;
    }

    private class DeleteRuleAction extends AbstractAction {
        private static final long serialVersionUID = 1L;
        private ReadableRule _rule = null;

        DeleteRuleAction(ReadableRule rule) {
            _rule = rule;
        }

        public void actionPerformed(ActionEvent e) {
            int resp = JOptionPane.showConfirmDialog(
                    controller.getEditorWindow(),
                    GDLEditorLanguageManager.getMessage("AskForRuleDeletionConfirmation"),
                    GDLEditorLanguageManager.getMessage("DeletingRule"),
                    JOptionPane.YES_NO_CANCEL_OPTION);
            if (resp == JOptionPane.YES_OPTION) {
                controller.getRenderableRules().remove(_rule.getGTCode());
                refresh();
            }
        }
    }

    private JButton createEditNameButton(ReadableRule rule) {
        JButton button = createGenericButton();
        button.setAction(new EditRuleNameAction(rule));
        button.setIcon(GDLEditorImageUtil.EDIT_ICON);
        button.setToolTipText(GDLEditorLanguageManager.getMessage("EditRuleName"));
        return button;
    }

    private class EditRuleNameAction extends AbstractAction {
        private static final long serialVersionUID = 1L;
        private ReadableRule _rule = null;

        EditRuleNameAction(ReadableRule rule) {
            _rule = rule;
        }

        public void actionPerformed(ActionEvent e) {
            DialogNameInsert dialog = new DialogNameInsert(
                    controller.getEditorWindow(),
                    GDLEditorLanguageManager.getMessage("EditRuleName"),
                    controller.getGTName(_rule.getGTCode()));
            if (dialog.getAnswer()) {
                controller.setGTName(_rule.getGTCode(), dialog.getValue());
                refresh();
            }
        }
    }

    private JButton createGenericButton() {
        JButton button = new JButton();
        button.setBorder(BorderFactory.createEmptyBorder());
        button.setContentAreaFilled(false);
        button.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        return button;
    }

    private class LoadRuleAction extends AbstractAction {
        private static final long serialVersionUID = 1L;
        private ReadableRule _rule = null;

        LoadRuleAction(ReadableRule rule) {
            _rule = rule;
        }

        public void actionPerformed(ActionEvent e) {
            controller.ruleEdit(_rule);
        }
    }

    private JScrollPane getRuleListJScrollPane() {
        if (ruleListJScrollPane == null) {
            ruleListJScrollPane = new JScrollPane();
            ruleListJScrollPane.setViewportView(getRuleListPanel());
        }
        return ruleListJScrollPane;
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