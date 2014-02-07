package se.cambio.cds.gdl.editor.view.panels;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.menubar.LoadGuideAction;
import se.cambio.cds.gdl.editor.view.menubar.SaveGuideAction;
import se.cambio.cds.gdl.editor.view.panels.interfaces.RefreshablePanel;
import se.cambio.cds.gdl.model.readable.rule.ReadableRule;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

public class GDLEditorMainPanel extends JPanel implements RefreshablePanel{
    /**
     *
     */
    private static final long serialVersionUID = 7045006987399987315L;
    private JButton saveButton = null;
    private JButton backToGuideButton = null;
    private JButton generateFormButton = null;
    private JButton addRuleButton;
    private JButton createBinding = null;
    private JPanel _mainPanel = null;
    private GuidePanel _guidePanel = null;
    private GDLEditor _controller = null;
    private JLabel _titleLabel = null;
    private JButton loadButton;
    private RulePanel _currentRulePanel = null;

    /**
     * This is the default constructor
     */
    public GDLEditorMainPanel(GDLEditor controller){
        _controller = controller;
        init();
    }

    /**
     * This method initializes this
     */
    private  void init() {
	/* Enter KeyStroke */
        KeyStroke enter = KeyStroke.getKeyStroke( KeyEvent.VK_ENTER,0,true);
        registerKeyboardAction(null, enter, JComponent.WHEN_IN_FOCUSED_WINDOW);
        setLayout(new BorderLayout());

        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

        buttonPanel.add(getLoadButton());
        buttonPanel.add(getSaveButton());
        buttonPanel.add(getGenerateFormButton());
        buttonPanel.add(getAddRuleButton());
        buttonPanel.add(getCreateBindingButton());
        buttonPanel.add(getBackToGuideButton());
        //buttonPanel.add(getTitleLabel());

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

    public class AddRuleAction extends AbstractAction{

        private static final long serialVersionUID = -3085701867293096187L;

        public void actionPerformed(ActionEvent e) {
            RulesPanel rulesPanel = getGuidePanel().getRulesPanel();
            getGuidePanel().getGuideEditorTabPane().setSelectedComponent(rulesPanel);
            rulesPanel.addRule();
        }
    }

    private JButton getCreateBindingButton() {
        if (createBinding == null) {
            createBinding = new JButton();
            createBinding.setText(GDLEditorLanguageManager.getMessage("AddBinding"));
            createBinding.setIcon(GDLEditorImageUtil.ADD_ONTOLOGY_ICON);
            createBinding.setToolTipText(GDLEditorLanguageManager.getMessage("AddBindingD"));
            createBinding.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    BindingsPanel bindingPanel = getGuidePanel().getBindingPanel();
                    getGuidePanel().getGuideEditorTabPane().setSelectedComponent(bindingPanel);
                    bindingPanel.addTermTab();
                }
            });
        }
        return createBinding;
    }

    public JPanel getMainPanel(){
        if (_mainPanel==null){
            _mainPanel = new JPanel();
            _mainPanel.setLayout(new BorderLayout());
            _mainPanel.add(getGuidePanel());
        }
        return _mainPanel;
    }

    public void loadGuideView(){
        getMainPanel().removeAll();
        _currentRulePanel = null;
        //getTitleLabel().setText(_controller.getTitle());
        getMainPanel().add(getGuidePanel());
        setButtonsInRule(false);
        this.repaint();
        this.validate();
    }

    public void loadRuleView(ReadableRule rule){
        getMainPanel().removeAll();
        JPanel auxPanel = new JPanel(new BorderLayout());
        getTitleLabel().setText(_controller.getGTName(rule.getGTCode()));
        auxPanel.add(getTitleLabel(), BorderLayout.NORTH);
        auxPanel.add(createRulePanel(), BorderLayout.CENTER);
        getMainPanel().add(auxPanel);
        setButtonsInRule(true);
        this.repaint();
        this.validate();
    }

    public void refresh(){
        if (_currentRulePanel!=null){
            _currentRulePanel.refresh();
        }else{
            Component comp = getGuidePanel().getGuideEditorTabPane().getSelectedComponent();
            if (comp instanceof RefreshablePanel){
                ((RefreshablePanel)comp).refresh();
            }
        }
    }


    public GuidePanel getGuidePanel(){
        if (_guidePanel==null){
            _guidePanel = new GuidePanel(_controller);
        }
        return _guidePanel;
    }

    public RulePanel createRulePanel(){
        _currentRulePanel = new RulePanel(_controller);
        return _currentRulePanel;
    }

    /**
     * This method initializes jButton	
     *
     * @return javax.swing.JButton
     */
    public JButton getSaveButton() {
        if (saveButton == null) {
            saveButton = new JButton(new SaveGuideAction());
            saveButton.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        }
        return saveButton;
    }


    public JButton getLoadButton() {
        if (loadButton == null) {
            loadButton = new JButton(new LoadGuideAction());
            loadButton.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        }
        return loadButton;
    }

    public JLabel getTitleLabel(){
        if (_titleLabel==null){
            _titleLabel = new JLabel();
            _titleLabel.setBorder(BorderFactory.createEmptyBorder(0,5,0,0));
            Font f = new Font("Dialog", Font.BOLD, 18);
            _titleLabel.setFont(f);
        }
        return _titleLabel;
    }

    public class TestGuideAction extends AbstractAction{

        private static final long serialVersionUID = -3085701867293096187L;

        public void actionPerformed(ActionEvent e) {
            _controller.compile();
        }
    }

    public void showError(String errorMsg){
        JOptionPane.showMessageDialog(
                this,
                GDLEditorLanguageManager.getMessage("Error")+": "+errorMsg,
                GDLEditorLanguageManager.getMessage("Error"),
                JOptionPane.ERROR_MESSAGE);
    }

    public void compilationOk(){
        JOptionPane.showMessageDialog(
                this,
                GDLEditorLanguageManager.getMessage("CompilationOK"),
                GDLEditorLanguageManager.getMessage("CompilationOK"),
                JOptionPane.INFORMATION_MESSAGE);
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

    public void setButtonsInRule(boolean visible){
        getBackToGuideButton().setVisible(visible);
        getAddRuleButton().setVisible(!visible);
        getCreateBindingButton().setVisible(!visible);
    }

    public class BackToGuideAction extends AbstractAction{
        private static final long serialVersionUID = -3085701867293096187L;
        public void actionPerformed(ActionEvent e) {
            _controller.goBackToGuide();
        }
    }


    public JButton getGenerateFormButton() {
        if (generateFormButton == null) {
            generateFormButton = new JButton();
            generateFormButton.setAction(new GenerateFormAction());
            generateFormButton.setText(GDLEditorLanguageManager.getMessage("GenerateForm"));
            generateFormButton.setIcon(GDLEditorImageUtil.TEST_ICON);
            generateFormButton.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        }
        return generateFormButton;
    }

    public class GenerateFormAction extends AbstractAction{
        private static final long serialVersionUID = -3085701867293096187L;
        public void actionPerformed(ActionEvent e) {
            generateFormAction();
        }
    }

    private void generateFormAction(){
        _controller.generateForm();
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