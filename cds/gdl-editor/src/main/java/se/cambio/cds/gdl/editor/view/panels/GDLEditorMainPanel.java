package se.cambio.cds.gdl.editor.view.panels;

import se.cambio.cds.controller.guide.GuideExportPlugin;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.controller.GuidelineLoadManager;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.menubar.LoadGuideAction;
import se.cambio.cds.gdl.editor.view.menubar.SaveGuideAction;
import se.cambio.cds.gdl.graph.view.panel.GdlGraphManager;
import se.cambio.cds.gdl.model.readable.rule.ReadableRule;
import se.cambio.cds.util.export.html.GuideHTMLExporter;
import se.cambio.cds.view.swing.panel.interfaces.RefreshablePanel;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

public class GDLEditorMainPanel extends JPanel implements RefreshablePanel {

    private static final long serialVersionUID = 7045006987399987315L;
    private JButton saveButton = null;
    private JButton backToGuideButton = null;
    private JButton generateFormButton = null;
    private JButton addRuleButton;
    private JButton createBinding = null;
    private JPanel mainPanel = null;
    private GuidePanel guidePanel = null;
    private GDLEditor controller = null;
    private GuidelineLoadManager guidelineLoadManager;
    private JLabel titleLabel = null;
    private JButton loadButton;
    private RulePanel currentRulePanel = null;
    private GuideHTMLExporter guideHTMLExporter;
    private GdlGraphManager gdlGraphManager;
    private GuideExportPlugin guideExportPlugin;

    public GDLEditorMainPanel(
            GDLEditor controller,
            GuidelineLoadManager guidelineLoadManager,
            GuideHTMLExporter guideHTMLExporter,
            GdlGraphManager gdlGraphManager,
            GuideExportPlugin guideExportPlugin) {
        this.controller = controller;
        this.guidelineLoadManager = guidelineLoadManager;
        this.guideHTMLExporter = guideHTMLExporter;
        this.gdlGraphManager = gdlGraphManager;
        this.guideExportPlugin = guideExportPlugin;
        init();
    }

    private void init() {
        KeyStroke enter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, true);
        registerKeyboardAction(null, enter, JComponent.WHEN_IN_FOCUSED_WINDOW);
        setLayout(new BorderLayout());
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        buttonPanel.add(getLoadButton());
        buttonPanel.add(getSaveButton());
        buttonPanel.add(getAddRuleButton());
        buttonPanel.add(getCreateBindingButton());
        buttonPanel.add(getGenerateFormButton());
        buttonPanel.add(getBackToGuideButton());
        add(buttonPanel, BorderLayout.NORTH);
        add(getMainPanel(), BorderLayout.CENTER);

    }


    public JButton getAddRuleButton() {
        if (addRuleButton == null) {
            addRuleButton = new JButton();
            addRuleButton.setAction(new AddRuleAction());
            addRuleButton.setText(GDLEditorLanguageManager.getMessage("AddRule"));
            addRuleButton.setToolTipText(GDLEditorLanguageManager.getMessage("AddRuleDesc"));
            addRuleButton.setIcon(GDLEditorImageUtil.ADD_RULE_ICON);
            addRuleButton.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        }
        return addRuleButton;
    }

    public class AddRuleAction extends AbstractAction {

        private static final long serialVersionUID = -3085701867293096187L;

        public void actionPerformed(ActionEvent e) {
            RulesPanel rulesPanel = getGuidePanel().getRulesPanel();
            getGuidePanel().getGuideEditorTabPane().setSelectedComponent(rulesPanel);
            rulesPanel.addRule();
        }
    }

    public JButton getCreateBindingButton() {
        if (createBinding == null) {
            createBinding = new JButton();
            createBinding.setText(GDLEditorLanguageManager.getMessage("AddBinding"));
            createBinding.setIcon(GDLEditorImageUtil.ADD_ONTOLOGY_ICON);
            createBinding.setToolTipText(GDLEditorLanguageManager.getMessage("AddBindingD"));
            createBinding.addActionListener(e -> {
                MultipleBindingsPanel bindingPanel = getGuidePanel().getBindingPanel();
                getGuidePanel().getGuideEditorTabPane().setSelectedComponent(bindingPanel);
                bindingPanel.addTermTab();
            });
        }
        return createBinding;
    }

    private JPanel getMainPanel() {
        if (mainPanel == null) {
            mainPanel = new JPanel();
            mainPanel.setLayout(new BorderLayout());
            mainPanel.add(getGuidePanel());
        }
        return mainPanel;
    }

    public void loadGuideView() {
        getMainPanel().removeAll();
        currentRulePanel = null;
        getMainPanel().add(getGuidePanel());
        setButtonsInRule(false);
        this.repaint();
        this.validate();
    }

    public void loadRuleView(ReadableRule rule) {
        getMainPanel().removeAll();
        JPanel auxPanel = new JPanel(new BorderLayout());
        getTitleLabel().setText(controller.getGTName(rule.getGTCode()));
        auxPanel.add(getTitleLabel(), BorderLayout.NORTH);
        auxPanel.add(createRulePanel(), BorderLayout.CENTER);
        getMainPanel().add(auxPanel);
        setButtonsInRule(true);
        this.repaint();
        this.validate();
    }

    public void refresh() {
        if (currentRulePanel != null) {
            currentRulePanel.refresh();
        } else {
            Component comp = getGuidePanel().getGuideEditorTabPane().getSelectedComponent();
            if (comp instanceof RefreshablePanel) {
                ((RefreshablePanel) comp).refresh();
            }
        }
    }

    public GuidePanel getGuidePanel() {
        if (guidePanel == null) {
            guidePanel = new GuidePanel(controller, guideHTMLExporter, gdlGraphManager, guideExportPlugin);
        }
        return guidePanel;
    }

    private RulePanel createRulePanel() {
        currentRulePanel = new RulePanel(controller);
        return currentRulePanel;
    }

    public JButton getSaveButton() {
        if (saveButton == null) {
            saveButton = new JButton(new SaveGuideAction(controller));
            saveButton.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
            saveButton.setEnabled(false);
        }
        return saveButton;
    }


    private JButton getLoadButton() {
        if (loadButton == null) {
            loadButton = new JButton(new LoadGuideAction(
                    controller,
                    guidelineLoadManager));
            loadButton.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        }
        return loadButton;
    }

    private JLabel getTitleLabel() {
        if (titleLabel == null) {
            titleLabel = new JLabel();
            titleLabel.setBorder(BorderFactory.createEmptyBorder(0, 5, 0, 0));
            Font f = new Font("Dialog", Font.BOLD, 18);
            titleLabel.setFont(f);
        }
        return titleLabel;
    }

    private JButton getBackToGuideButton() {
        if (backToGuideButton == null) {
            backToGuideButton = new JButton();
            backToGuideButton.setAction(new BackToGuideAction());
            backToGuideButton.setText(GDLEditorLanguageManager.getMessage("BackToGuide"));
            backToGuideButton.setIcon(GDLEditorImageUtil.ARROW_BACK_ICON);
            backToGuideButton.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
            backToGuideButton.setVisible(false);
        }
        return backToGuideButton;
    }

    private void setButtonsInRule(boolean visible) {
        getBackToGuideButton().setVisible(visible);
        getAddRuleButton().setVisible(!visible);
        getCreateBindingButton().setVisible(!visible);
    }

    public class BackToGuideAction extends AbstractAction {
        private static final long serialVersionUID = -3085701867293096187L;

        public void actionPerformed(ActionEvent e) {
            controller.goBackToGuide();
        }
    }


    public JButton getGenerateFormButton() {
        if (generateFormButton == null) {
            generateFormButton = new JButton();
            generateFormButton.setAction(new GenerateFormAction());
            generateFormButton.setText(GDLEditorLanguageManager.getMessage("Run"));
            generateFormButton.setIcon(GDLEditorImageUtil.RUN_ICON);
            generateFormButton.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        }
        return generateFormButton;
    }

    public class GenerateFormAction extends AbstractAction {
        private static final long serialVersionUID = -3085701867293096187L;

        public void actionPerformed(ActionEvent e) {
            generateFormAction();
        }
    }

    private void generateFormAction() {
        controller.generateForm();
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