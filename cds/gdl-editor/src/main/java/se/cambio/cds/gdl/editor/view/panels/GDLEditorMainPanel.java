package se.cambio.cds.gdl.editor.view.panels;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import se.cambio.cds.controller.guide.GuideManager;
import se.cambio.cds.formgen.controller.FormGeneratorController;
import se.cambio.cds.formgen.view.dialog.CDSFormGenDialog;
import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.controller.sw.CompileGuideSW;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.LanguageManager;
import se.cambio.cds.gdl.editor.view.menubar.LoadGuideAction;
import se.cambio.cds.gdl.editor.view.menubar.SaveGuideAction;
import se.cambio.cds.gdl.model.readable.rule.ReadableRule;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.util.UserConfigurationManager;

public class GDLEditorMainPanel extends JPanel{
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
	    addRuleButton.setText(LanguageManager.getMessage("AddRule"));
	    addRuleButton.setToolTipText(LanguageManager.getMessage("AddRuleDesc"));
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
	    createBinding.setText(LanguageManager.getMessage("AddBinding"));
	    createBinding.setIcon(GDLEditorImageUtil.ADD_ONTOLOGY_ICON);
	    createBinding.setToolTipText(LanguageManager.getMessage("AddBindingD"));
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
	getTitleLabel().setText(_controller.getTitle());
	getMainPanel().add(getGuidePanel());
	setButtonsInGuide(false);
	this.repaint();
	this.validate();
    }

    public void loadRuleView(ReadableRule rule){
	getTitleLabel().setText(_controller.getTitle()+" > "+_controller.getGTName(rule.getGTCode()));
	getMainPanel().removeAll();
	getMainPanel().add(createRulePanel());
	setButtonsInGuide(true);
	this.repaint();
	this.validate();
    }

    public GuidePanel getGuidePanel(){
	if (_guidePanel==null){
	    _guidePanel = new GuidePanel(_controller);	
	}
	return _guidePanel;
    }

    public RulePanel createRulePanel(){
	return new RulePanel(_controller);
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
	    Font f = new Font("Dialog", Font.PLAIN, 18);
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
		LanguageManager.getMessage("Error")+": "+errorMsg,
		LanguageManager.getMessage("Error"),
		JOptionPane.ERROR_MESSAGE);
    }

    public void compilationOk(){
	JOptionPane.showMessageDialog(
		this,
		LanguageManager.getMessage("CompilationOK"),
		LanguageManager.getMessage("CompilationOK"),
		JOptionPane.INFORMATION_MESSAGE);
    }


    private JButton getBackToGuideButton() {
	if (backToGuideButton == null) {
	    backToGuideButton = new JButton();
	    backToGuideButton.setAction(new BackToGuideAction());
	    backToGuideButton.setText(LanguageManager.getMessage("BackToGuide"));
	    backToGuideButton.setIcon(GDLEditorImageUtil.ARROW_BACK_ICON);
	    backToGuideButton.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
	    backToGuideButton.setVisible(false);
	}
	return backToGuideButton;
    }

    public void setButtonsInGuide(boolean visible){
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
	    generateFormButton.setText(LanguageManager.getMessage("GenerateForm"));
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
	if (!_controller.hasActiveRules()){
	    JOptionPane.showInternalMessageDialog(this, LanguageManager.getMessage("PleaseInsertRulesBeforeGeneratingForm"), LanguageManager.getMessage("GenerateForm"), JOptionPane.WARNING_MESSAGE);
	    return;
	}
	String gdlGuide = _controller.serializeCurrentGuide();
	if (gdlGuide!=null){
	    CompileGuideSW sw = new CompileGuideSW(_controller){
		protected void done() {
		    getController().compilationFinished(getErrorMsg());
		    if (getErrorMsg()==null){
			generateForm(getCompiledGuide());
		    }
		}
	    };
	    sw.execute();
	    _controller.setBusy(LanguageManager.getMessage("Compiling")+"...");
	}
    }

    public void generateForm(byte[] compiledGuide){
	GDLEditor controller = EditorManager.getActiveGDLEditor();
	String gdlGuide = controller.serializeCurrentGuide();
	if (compiledGuide!=null && gdlGuide!=null){
	    GuideDTO guideDTO = 
		    new GuideDTO(
			    controller.getIdGuide(), 
			    controller.getTitle(),
			    controller.getTitle(),
			    gdlGuide,
			    compiledGuide);
	    Collection<GuideDTO> guides = new ArrayList<GuideDTO>();
	    guides.add(guideDTO);
	    GuideManager guideManager = new GuideManager(guides);
	    FormGeneratorController formGenerator = 
		    new FormGeneratorController(guideDTO, guideManager, controller.getCurrentGuideLanguageCode());
	    Date date = UserConfigurationManager.getCustomDate();
	    if (date!=null){
		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		formGenerator.setCurrentDate(cal);
	    }
	    CDSFormGenDialog dialog = new CDSFormGenDialog(EditorManager.getActiveEditorWindow());
	    dialog.setFormGeneratorController(formGenerator);
	    //dialog.addWindowListener(new CloseAction());
	    dialog.setVisible(true);
	}
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