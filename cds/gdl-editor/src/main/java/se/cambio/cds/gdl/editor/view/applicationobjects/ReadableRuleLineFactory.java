package se.cambio.cds.gdl.editor.view.applicationobjects;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.labels.JLinkRuleElementLabel;
import se.cambio.cds.gdl.editor.view.listeners.PluginTypeLinkListener;
import se.cambio.cds.gdl.editor.view.listeners.RuleLineElementItemListener;
import se.cambio.cds.gdl.editor.view.panels.RuleLinesPanel;
import se.cambio.cds.gdl.editor.view.panels.rulelinecontainers.*;
import se.cambio.cds.gdl.editor.view.renderers.SingleSelectionRuleElementRenderer;
import se.cambio.cds.gdl.model.readable.rule.lines.OrOperatorRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ExpressionRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.RuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.RuleLineElementWithValue;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.SingleSelectionRuleElement;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.InstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.OperatorRuleLine;
import se.cambio.cds.gdl.model.readable.util.ExpressionUtil;
import se.cambio.openehr.util.UserConfigurationManager;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;

public class ReadableRuleLineFactory {

    public static RuleLineContainerPanel createRuleLineContainer(RuleLinesPanel ruleLinesPanel, RuleLine ruleLine, GDLEditor gdlEditor) {
        if (ruleLine instanceof InstantiationRuleLine) {
            MultipleRuleLineContainerWithHeader archetypeInstantiationRL = new MultipleRuleLineContainerWithHeader(ruleLinesPanel, ruleLine, gdlEditor);
            archetypeInstantiationRL.setBorder(BorderFactory.createEmptyBorder(0, 0, 8, 0));
            return archetypeInstantiationRL;
        } else if (ruleLine instanceof OrOperatorRuleLine) {
            return new OrOperatorRuleLinePanel(ruleLinesPanel, (OrOperatorRuleLine) ruleLine, gdlEditor);
        } else if (ruleLine instanceof OperatorRuleLine) {
            return new OperatorRuleLineContainer(ruleLinesPanel, ruleLine, gdlEditor);
        } else {
            return new SingleRuleLinePanel(ruleLinesPanel, ruleLine, gdlEditor);
        }
    }


    @SuppressWarnings({"unchecked", "rawtypes"})
    public static JPanel createRuleLinePanel(RuleLinesPanel ruleLinesPanel, RuleLine ruleLine, GDLEditor gdlEditor) {
        JPanel jPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        JLabel iconLabel = new JLabel(RuleLineDirectory.getIconForRuleLine(ruleLine));
        if (ruleLine.isCommented()) {
            iconLabel.setEnabled(false);
        }
        jPanel.add(iconLabel);
        jPanel.add(Box.createHorizontalStrut(3));
        jPanel.addMouseListener(ruleLinesPanel.getSelectableRuleLineDragMouseListener());
        jPanel.addMouseMotionListener(ruleLinesPanel.getSelectableRuleLineDragMouseListener());

        for (RuleLineElement ruleLineElement : ruleLine.getRuleLineElements()) {
            if (ruleLineElement instanceof RuleLineElementWithValue) {
                RuleLineElementWithValue<?> ruleLineElementWithValue =
                        (RuleLineElementWithValue<?>) ruleLineElement;
                if (ruleLineElement instanceof SingleSelectionRuleElement) {
                    SingleSelectionRuleElement ssre = (SingleSelectionRuleElement) ruleLineElement;
                    JComboBox comboBox = new JComboBox();
                    for (Object item : ssre.getItems()) {
                        comboBox.addItem(item);
                    }
                    if (ruleLineElementWithValue.getValue() != null) {
                        comboBox.setSelectedItem(ruleLineElementWithValue.getValue());
                    } else {
                        comboBox.setEditable(true);
                        comboBox.setSelectedItem(null);
                        comboBox.setEditable(false);
                    }
                    comboBox.setRenderer(new SingleSelectionRuleElementRenderer(ssre));
                    comboBox.addItemListener(new RuleLineElementItemListener(ruleLineElementWithValue));
                    jPanel.add(comboBox);
                } else {
                    JLinkRuleElementLabel linkLabel =
                            new JLinkRuleElementLabel(ruleLineElementWithValue);
                    linkLabel.setToolTipText(ruleLineElement.getLabelDescription());
                    linkLabel.setCommented(ruleLine.isCommented());
                    linkLabel.refresh();
                    linkLabel.addActionListener(new PluginTypeLinkListener(ruleLinesPanel));
                    jPanel.add(linkLabel);
                }
            } else {
                jPanel.add(createLabel(ruleLine, ruleLineElement));
            }
            jPanel.add(Box.createHorizontalStrut(4));
        }
        jPanel.add(createCommentButton(ruleLine, ruleLinesPanel));
        jPanel.add(Box.createHorizontalStrut(2));
        jPanel.add(createDeleteButton(ruleLine, ruleLinesPanel, gdlEditor));
        return jPanel;
    }

    private static JLabel createLabel(RuleLine ruleLine, RuleLineElement ruleLineElement) {
        String text;
        if (ruleLineElement instanceof ExpressionRuleLineElement) {
            ExpressionRuleLineElement erli = (ExpressionRuleLineElement) ruleLineElement;
            text = ExpressionUtil.convertToHTMLText(erli, erli.getValue(), UserConfigurationManager.instance().getLanguage());
        } else {
            text = ruleLineElement.getLabelText();
        }
        text = "<HTML>" + text + "</HTML>";
        JLabel label = new JLabel(text);
        if (ruleLine.isCommented()) {
            label.setForeground(Color.GRAY);
        } else if (ruleLineElement instanceof RuleLineElementWithValue
                && !(ruleLineElement instanceof SingleSelectionRuleElement)) {
            label.setForeground(Color.BLUE);
        }
        return label;
    }

    public static JButton createDeleteButton(RuleLine ruleLine, RuleLinesPanel ruleLinesPanel, GDLEditor gdlEditor) {
        JButton button = createGenericButton();
        button.setAction(new DeleteAction(ruleLine, ruleLinesPanel, gdlEditor));
        button.setIcon(GDLEditorImageUtil.DELETE_ICON);
        button.setToolTipText(GDLEditorLanguageManager.getMessage("DeleteLine"));
        return button;
    }

    private static class DeleteAction extends AbstractAction {
        private static final long serialVersionUID = 1L;
        private RuleLine ruleLine = null;
        private RuleLinesPanel ruleLinesPanel = null;
        private GDLEditor gdlEditor;

        DeleteAction(RuleLine ruleLine, RuleLinesPanel ruleLinesPanel, GDLEditor gdlEditor) {
            this.ruleLine = ruleLine;
            this.ruleLinesPanel = ruleLinesPanel;
            this.gdlEditor = gdlEditor;
        }

        public void actionPerformed(ActionEvent e) {
            if (gdlEditor.checkRuleLineDelete(ruleLine)) {
                int resp = JOptionPane.showConfirmDialog(
                        gdlEditor.getEditorWindow(),
                        GDLEditorLanguageManager.getMessage("AskForRuleLineDeletionConfirmation"),
                        GDLEditorLanguageManager.getMessage("DeletingRuleLine"),
                        JOptionPane.YES_NO_CANCEL_OPTION);
                if (resp == JOptionPane.YES_OPTION) {


                    if (ruleLine.getParentRuleLine() != null) {
                        ruleLine.detachFromParent();
                    } else {
                        ruleLinesPanel.removeRuleLine(ruleLine);
                    }
                    ruleLinesPanel.refresh();
                }
            }
        }
    }

    public static JButton createCommentButton(RuleLine ruleLine, RuleLinesPanel ruleLinesPanel) {
        JButton button = createGenericButton();
        button.setAction(new ChangeCommentAction(ruleLine, ruleLinesPanel));
        if (ruleLine.isCommented()) {
            button.setIcon(GDLEditorImageUtil.UNACCEPT_ICON);
        } else {
            button.setIcon(GDLEditorImageUtil.ACCEPT_ICON);
        }
        button.setToolTipText(GDLEditorLanguageManager.getMessage("SetActiveInactive"));
        return button;
    }

    private static class ChangeCommentAction extends AbstractAction {
        private static final long serialVersionUID = 1L;
        private RuleLine _ruleLine = null;
        private RuleLinesPanel _ruleLinesPanel = null;

        ChangeCommentAction(RuleLine ruleLine, RuleLinesPanel ruleLinesPanel) {
            _ruleLine = ruleLine;
            _ruleLinesPanel = ruleLinesPanel;
        }

        public void actionPerformed(ActionEvent e) {
            _ruleLine.setCommented(!_ruleLine.isCommented());
            _ruleLinesPanel.refresh();
        }
    }

    private static JButton createGenericButton() {
        JButton button = new JButton();
        button.setBorder(BorderFactory.createEmptyBorder());
        button.setContentAreaFilled(false);
        button.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        return button;
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