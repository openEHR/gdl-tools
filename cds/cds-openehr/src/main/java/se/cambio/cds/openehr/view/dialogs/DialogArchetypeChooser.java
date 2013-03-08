package se.cambio.cds.openehr.view.dialogs;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.Collections;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.KeyStroke;
import javax.swing.ListCellRenderer;

import se.cambio.cds.model.archetype.dto.ArchetypeDTO;
import se.cambio.cds.model.template.dto.TemplateDTO;
import se.cambio.cds.openehr.util.ImageUtil;
import se.cambio.cds.openehr.util.OpenEHRConst;
import se.cambio.cds.openehr.util.OpenEHRLanguageManager;
import se.cambio.cds.openehr.view.applicationobjects.Archetypes;
import se.cambio.cds.openehr.view.applicationobjects.DomainsUI;
import se.cambio.cds.openehr.view.applicationobjects.Templates;
import se.cambio.cds.openehr.view.comparators.ArchetypeComparator;
import se.cambio.cds.openehr.view.comparators.TemplateComparator;
import se.cambio.cds.openehr.view.panels.SelectionPanel;
import se.cambio.cds.openehr.view.trees.SelectableNode;
import se.cambio.cds.openehr.view.trees.SelectableNodeWithIcon;
import se.cambio.cds.openehr.view.util.ImportUtils;
import se.cambio.cds.openehr.view.util.NodeConversor;
import se.cambio.cds.util.AggregationFunctions;
import se.cambio.cds.util.Domains;


public class DialogArchetypeChooser extends JDialog{
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private JButton acceptButton;
    private JButton cancelButton;
    private JPanel jPanel;
    private JPanel bottonPanel;
    private boolean _answer = false;
    private AcceptChangesAction acceptChangesAction;
    private CancelChangesAction cancelChangesAction;
    private SelectionPanel archetypeSelectionPanel;
    private SelectionPanel templateSelectionPanel;
    private SelectableNode<Object> archetypeNode = null;
    private SelectableNode<Object> templateNode = null;
    private JComboBox domainComboBox;
    private String _currentAggregationFunction = null;
    private JTabbedPane tabbedPane;
    private JCheckBox lastCB;

    private String ANY_DOMAIN = "*";

    public DialogArchetypeChooser(Window owner){
	super(owner, 
		OpenEHRLanguageManager.getMessage("Archetypes")+"/"+OpenEHRLanguageManager.getMessage("Templates"),
		ModalityType.APPLICATION_MODAL);
	init(new Dimension(500,500), false);
    }


    public DialogArchetypeChooser(Window owner, String archetypeId, String domainId,boolean selectTemplates, String aggregationFunction, boolean onlyShowCDS){
	super(
		owner, 
		OpenEHRLanguageManager.getMessage("Archetypes")+"/"+OpenEHRLanguageManager.getMessage("Templates"), 
		ModalityType.APPLICATION_MODAL);
	_currentAggregationFunction = aggregationFunction;
	if (onlyShowCDS){
	    getComboBox().setSelectedItem(Domains.CDS_ID);
	    getComboBox().setEnabled(false);
	}else if (archetypeId!=null){
	    if (domainId==null){
		domainId = ANY_DOMAIN;
	    }
	    if (!Domains.EHR_ID.equals(domainId)){//Select only if it is not EHR (the one by default) because it will activate the 'Last' ComboBox
		getComboBox().setSelectedItem(domainId);
	    }
	}
	getLastCB().setVisible(Domains.EHR_ID.equals( getComboBox().getSelectedItem()));
	init(new Dimension(500,500), selectTemplates);
    }

    private void init(Dimension size, boolean selectTemplates){
	Dimension screenSize =
		Toolkit.getDefaultToolkit().getScreenSize();
	Dimension labelSize = this.getSize();
	this.setSize(size);
	int locx = (screenSize.width/2) - (labelSize.width/2) - (this.getWidth()/2);
	int locy = (screenSize.height/2) - (labelSize.height/2) - (this.getHeight()/2);
	this.setLocation(locx,locy);
	this.setResizable(true);
	this.addWindowListener(getCancelChangesAction());
	this.setContentPane(getJPanel());
	/* Enter KeyStroke */
	KeyStroke enter = KeyStroke.getKeyStroke( KeyEvent.VK_ENTER,0,true);
	getJPanel().registerKeyboardAction(getAcceptChangesAction(), enter, JComponent.WHEN_IN_FOCUSED_WINDOW);
	KeyStroke esc = KeyStroke.getKeyStroke( KeyEvent.VK_ESCAPE,0,true);
	getJPanel().registerKeyboardAction(getCancelChangesAction(), esc, JComponent.WHEN_IN_FOCUSED_WINDOW);
	if (selectTemplates){
	    getArchetypeTemplateTabbedPane().setSelectedIndex(1);
	}else{
	    getArchetypeTemplateTabbedPane().setSelectedIndex(0);
	}
    }

    private JPanel getJPanel() {
	if (jPanel == null) {
	    jPanel = new JPanel(new BorderLayout());
	    JPanel aux = new JPanel(new FlowLayout(FlowLayout.LEFT));
	    aux.add(new JLabel(OpenEHRLanguageManager.getMessage("Domain")+":"));
	    aux.add(getComboBox());
	    jPanel.add(aux, BorderLayout.NORTH);
	    jPanel.add(getArchetypeTemplateTabbedPane(), BorderLayout.CENTER);
	    JPanel panelAux = new JPanel(new BorderLayout());
	    jPanel.add(panelAux, BorderLayout.SOUTH);
	    panelAux.add(getLastCB(), BorderLayout.NORTH);
	    panelAux.add(getBottonPanel(), BorderLayout.SOUTH);
	}
	return jPanel;
    }

    private JComboBox getComboBox(){
	if (domainComboBox==null){
	    domainComboBox = new JComboBox();
	    domainComboBox.addItem(ANY_DOMAIN);
	    domainComboBox.addItem(Domains.EHR_ID);
	    domainComboBox.addItem(Domains.CDS_ID);
	    domainComboBox.setRenderer(new DomainComboBoxRenderer());
	    domainComboBox.setSelectedItem(Domains.EHR_ID);
	    domainComboBox.addItemListener(new ItemListener() {
		public void itemStateChanged(ItemEvent e) {
		    boolean isEHR = Domains.EHR_ID.equals(e.getItem());
		    getLastCB().setSelected(isEHR);
		    getLastCB().setVisible(isEHR);
		}
	    });
	}
	return domainComboBox;
    }

    private class DomainComboBoxRenderer extends JLabel implements ListCellRenderer{

	private static final long serialVersionUID = 1L;

	@Override
	public Component getListCellRendererComponent(JList list, Object value,
		int index, boolean isSelected, boolean cellHasFocus) {
	    if (isSelected) {
		setBackground(list.getSelectionBackground());
		setForeground(list.getSelectionForeground());
	    }else {
		setBackground(list.getBackground());
		setForeground(list.getForeground());
	    }
	    if (value instanceof String){
		String idDomain = (String)value;
		if (idDomain.equals(ANY_DOMAIN)){
		    idDomain = null;
		}
		setText(DomainsUI.getName(idDomain));
		setToolTipText(DomainsUI.getDescription(idDomain));
		setIcon(DomainsUI.getIcon(idDomain));
	    }else{
		setText(null);
		setToolTipText(null);
		setIcon(null);
	    }
	    return this;
	}
    }

    private JTabbedPane getArchetypeTemplateTabbedPane(){
	if (tabbedPane == null){
	    tabbedPane = new JTabbedPane();
	    refreshArchetypeSelectionPanel();
	    refreshTemplateSelectionPanel();
	}
	return tabbedPane;
    }

    private JPanel getArchetypeSelectionPanel(){
	if(archetypeSelectionPanel==null){
	    archetypeSelectionPanel = new SelectionPanel(getArchetypeNode());
	    archetypeSelectionPanel.getJTree().expand(getArchetypeNode());
	    archetypeSelectionPanel.getJTree().addExtraMouseListener(new DoubleClickMouseListener());
	    JButton addArchetypeButton = new JButton(OpenEHRLanguageManager.getMessage("Import"));
	    addArchetypeButton.setIcon(ImageUtil.FOLDER_ICON);
	    addArchetypeButton.addActionListener(new ImportArchetypeActionListener(this));
	    archetypeSelectionPanel.getFilterPanel().add(addArchetypeButton);
	}
	return archetypeSelectionPanel;
    }

    private class ImportArchetypeActionListener implements ActionListener{
	private DialogArchetypeChooser _dialog = null;
	public ImportArchetypeActionListener(DialogArchetypeChooser dialog){
	    _dialog = dialog;
	}

	public void actionPerformed(ActionEvent e) {
	    ImportUtils.showImportArchetypeDialog(_dialog, null);
	    refreshArchetypeSelectionPanel();
	}
    }



    public void refreshArchetypeSelectionPanel(){
	archetypeSelectionPanel = null;
	archetypeNode = null;
	getArchetypeSelectionPanel();
	if (getArchetypeTemplateTabbedPane().getTabCount()>0){
	    getArchetypeTemplateTabbedPane().removeTabAt(0);
	}
	getArchetypeTemplateTabbedPane().insertTab(OpenEHRLanguageManager.getMessage("Archetypes"), Archetypes.ICON, getArchetypeSelectionPanel(), OpenEHRLanguageManager.getMessage("Archetypes"), 0);
	getArchetypeTemplateTabbedPane().setSelectedIndex(0);
    }

    private JPanel getTemplateSelectionPanel(){
	if(templateSelectionPanel==null){
	    templateSelectionPanel = new SelectionPanel(getTemplateNode());
	    templateSelectionPanel.getJTree().expand(getTemplateNode());
	    templateSelectionPanel.getJTree().addExtraMouseListener(new DoubleClickMouseListener());
	    JButton addTemplateButton = new JButton(OpenEHRLanguageManager.getMessage("Import"));
	    addTemplateButton.setIcon(ImageUtil.FOLDER_ICON);
	    addTemplateButton.addActionListener(new ImportTemplateActionListener(this));
	    templateSelectionPanel.getFilterPanel().add(addTemplateButton);
	}
	return templateSelectionPanel;
    }

    private class ImportTemplateActionListener implements ActionListener{
	private DialogArchetypeChooser _dialog = null;
	public ImportTemplateActionListener(DialogArchetypeChooser dialog){
	    _dialog = dialog;
	}

	public void actionPerformed(ActionEvent e) {
	    ImportUtils.showImportTemplateDialog(_dialog, null);
	    refreshTemplateSelectionPanel();
	}
    }

 

  

    public void refreshTemplateSelectionPanel(){
	templateSelectionPanel = null;
	templateNode = null;
	getTemplateSelectionPanel();
	if (getArchetypeTemplateTabbedPane().getTabCount()>1){
	    getArchetypeTemplateTabbedPane().removeTabAt(1);
	}
	getArchetypeTemplateTabbedPane().insertTab(OpenEHRLanguageManager.getMessage("Templates"), Templates.ICON, getTemplateSelectionPanel(), OpenEHRLanguageManager.getMessage("Templates"), 1);
	getArchetypeTemplateTabbedPane().setSelectedIndex(1);
    }

    class DoubleClickMouseListener extends MouseAdapter{
	public void mouseClicked(MouseEvent e) {
	    if(e.getClickCount()>1){
		if (getSelectedArchetypeId()!=null){
		    accept();
		}
	    }
	}
    }

    protected final void accept(){
	_answer  = true;
	setVisible(false);
    }

    protected final void exit(){
	_answer = false;
	setVisible(false);
    }

    private JPanel getBottonPanel(){
	if (bottonPanel==null){
	    bottonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
	    bottonPanel.add(getAcceptButton());
	    bottonPanel.add(getCancelButton());
	}
	return bottonPanel;
    }
    private JButton getAcceptButton() {
	if (acceptButton == null) {
	    acceptButton = new JButton();
	    acceptButton.setText(OpenEHRLanguageManager.getMessage("Accept"));
	    acceptButton.setIcon(ImageUtil.ACCEPT_ICON);
	    acceptButton.setEnabled(true);
	    acceptButton.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
	    acceptButton.addActionListener(getAcceptChangesAction());
	}
	return acceptButton;
    }

    /**
     * This method initializes cancelButton	
     * 	
     * @return javax.swing.JButton	
     */    
    private JButton getCancelButton() {
	if (cancelButton == null) {
	    cancelButton = new JButton();
	    cancelButton.setText(OpenEHRLanguageManager.getMessage("Cancel"));
	    cancelButton.setIcon(ImageUtil.CANCEL_ICON);
	    cancelButton.setEnabled(true);
	    cancelButton.addActionListener(getCancelChangesAction());
	}
	return cancelButton;
    }

    private AcceptChangesAction getAcceptChangesAction(){
	if (acceptChangesAction == null){
	    acceptChangesAction = new AcceptChangesAction();
	}
	return acceptChangesAction;
    }

    public class AcceptChangesAction extends AbstractAction{
	private static final long serialVersionUID = -8058749276509227718L;

	public void actionPerformed(ActionEvent e) {
	    accept();
	}
    }

    private CancelChangesAction getCancelChangesAction(){
	if (cancelChangesAction == null){
	    cancelChangesAction = new CancelChangesAction();
	}
	return cancelChangesAction;
    }

    private class CancelChangesAction extends WindowAdapter implements ActionListener{

	public void windowOpened(WindowEvent e){
	}

	public void actionPerformed(ActionEvent e) {
	    exit();
	}

	public void windowClosing(WindowEvent we) {
	    exit();
	}
    }

    public SelectableNode<Object> getArchetypeNode(){
	if(archetypeNode==null){
	    archetypeNode = new SelectableNodeWithIcon<Object>(
		    OpenEHRLanguageManager.getMessage("Archetypes"),
		    null, true, false,
		    Archetypes.ICON
		    );
	    insertArchetypeNodes(archetypeNode, OpenEHRConst.OBSERVATION);
	    insertArchetypeNodes(archetypeNode, OpenEHRConst.ACTION);
	    insertArchetypeNodes(archetypeNode, OpenEHRConst.EVALUATION);
	    insertArchetypeNodes(archetypeNode, OpenEHRConst.INSTRUCTION);
	    insertArchetypeNodes(archetypeNode, OpenEHRConst.COMPOSITION);
	    insertArchetypeNodes(archetypeNode, OpenEHRConst.CLUSTER);
	    insertArchetypeNodes(archetypeNode, OpenEHRConst.ITEM_TREE);
	    insertArchetypeNodes(archetypeNode, OpenEHRConst.ITEM_LIST);
	    insertArchetypeNodes(archetypeNode, OpenEHRConst.ITEM_TABLE);
	    insertArchetypeNodes(archetypeNode, OpenEHRConst.ITEM_SINGLE);
	}
	return archetypeNode;
    }

    private static void insertArchetypeNodes(SelectableNode<Object> root, String rmName){

	SelectableNode<Object> entryRoot = new SelectableNodeWithIcon<Object>(
		OpenEHRConst.getName(rmName),
		null, true, false,
		OpenEHRConst.getIcon(rmName),
		OpenEHRConst.getDescription(rmName));
	SelectableNode<Object> nodoOrigen = null;
	ArrayList<ArchetypeDTO> archetypeVOs = Archetypes.getArchetypes(rmName);
	Collections.sort(archetypeVOs, new ArchetypeComparator());
	for (ArchetypeDTO archetypeVO : archetypeVOs) {
	    nodoOrigen = new SelectableNodeWithIcon<Object>(
		    archetypeVO.getName(),archetypeVO,true, 
		    false, 
		    Archetypes.getIcon(archetypeVO.getIdArchetype()),
		    archetypeVO.getDescription());
	    entryRoot.add(nodoOrigen);
	}
	root.add(entryRoot);

    }

    public SelectableNode<Object> getTemplateNode(){
	if (templateNode==null){
	    templateNode = new SelectableNodeWithIcon<Object>(
		    OpenEHRLanguageManager.getMessage("Templates"),
		    null, true, false,
		    Templates.ICON
		    );
	    insertTemplateNodes(templateNode, OpenEHRConst.OBSERVATION);
	    insertTemplateNodes(templateNode, OpenEHRConst.ACTION);
	    insertTemplateNodes(templateNode, OpenEHRConst.EVALUATION);
	    insertTemplateNodes(templateNode, OpenEHRConst.INSTRUCTION);
	    insertTemplateNodes(templateNode, OpenEHRConst.COMPOSITION);
	}
	return templateNode;
    }

    private static void insertTemplateNodes(SelectableNode<Object> root, String rmName){
	SelectableNode<Object> entryRoot = new SelectableNodeWithIcon<Object>(
		OpenEHRConst.getName(rmName),
		null, true, false,
		OpenEHRConst.getIcon(rmName),
		OpenEHRConst.getDescription(rmName));
	SelectableNode<Object> nodoOrigen = null;
	ArrayList<TemplateDTO> templateVOs = Templates.getTemplates(rmName);
	Collections.sort(templateVOs, new TemplateComparator());
	for (TemplateDTO templateVO : templateVOs) {
	    nodoOrigen = new SelectableNodeWithIcon<Object>(
		    templateVO.getIdTemplate()+" - "+templateVO.getName(),templateVO,true, 
		    false, 
		    Archetypes.getIcon(templateVO.getIdArchetype()),
		    templateVO.getDescription());
	    entryRoot.add(nodoOrigen);
	}
	root.add(entryRoot);
    }

    private JCheckBox getLastCB(){
	if (lastCB==null){
	    lastCB = new JCheckBox(OpenEHRLanguageManager.getMessage("OnlyMostRecent"));
	    lastCB.setSelected(_currentAggregationFunction!=null && _currentAggregationFunction.equals(AggregationFunctions.ID_AGGREGATION_FUNCTION_LAST));
	}
	return lastCB;
    }

    public boolean getAnswer(){
	return _answer;
    }

    public String getSelectedArchetypeId(){
	Object selected = null;
	if (getArchetypeTemplateTabbedPane().getSelectedIndex()==0){
	    selected = NodeConversor.getSelectedObject(getArchetypeNode());
	    if (selected instanceof ArchetypeDTO){
		return ((ArchetypeDTO)selected).getIdArchetype();
	    }
	}else{
	    selected = NodeConversor.getSelectedObject(getTemplateNode());
	    if (selected instanceof TemplateDTO){
		return ((TemplateDTO)selected).getIdArchetype();
	    }
	}
	return null;
    }


    public String getSelectedTemplateId(){
	Object selected = null;
	if (getArchetypeTemplateTabbedPane().getSelectedIndex()==0){
	    return null;
	}else{
	    selected = NodeConversor.getSelectedObject(getTemplateNode());
	    if (selected instanceof TemplateDTO){
		return ((TemplateDTO)selected).getIdTemplate();
	    }
	}
	return null;
    }

    public String getSelectedDomain(){
	String idDomain = (String)getComboBox().getSelectedItem();
	if (idDomain.equals(ANY_DOMAIN)){
	    idDomain = null;
	}
	return idDomain;
    }


    public String getSelectedAggregationFunction(){
	return getLastCB().isSelected()?AggregationFunctions.ID_AGGREGATION_FUNCTION_LAST:null;
    }
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  1.1 (the 'License'); you may not use this file except in compliance with
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