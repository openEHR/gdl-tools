package se.cambio.cds.gdl.editor.view.panels;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.openehr.rm.datatypes.text.CodePhrase;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.panels.interfaces.RefreshablePanel;
import se.cambio.cds.gdl.editor.view.tables.BindingTable;
import se.cambio.cds.gdl.editor.view.tables.BindingTable.BindingTableModel;
import se.cambio.cds.gdl.model.Binding;
import se.cambio.cds.gdl.model.TermBinding;

public class BindingPanel  extends JPanel implements RefreshablePanel {
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private GDLEditor _controller = null;
    private JScrollPane mainScrollPanel;
    private BindingTable bindingTable;
    private String _terminologyId = null;
    private JButton addTermBtn = null;
    private JButton deleteBtn = null;
    private JPanel buttonPanel;

    public BindingPanel(GDLEditor gdlEditor, String terminologyId){
	_controller = gdlEditor;
	_terminologyId = terminologyId;
	init();
    }

    public void init(){
	this.setLayout(new BorderLayout());
	refresh();
    }

    private JScrollPane getMainScrollPanel(){
	if (mainScrollPanel==null){
	    mainScrollPanel = new JScrollPane();
	    mainScrollPanel.setViewportView(getBindingTable());
	}
	return mainScrollPanel;
    }

    public BindingTable getBindingTable(){
	if (bindingTable==null){
	    bindingTable = 
		    new BindingTable(
			    _controller.getTermBindings().get(_terminologyId).getBindings(),
			    _terminologyId);
	}
	return bindingTable;
    }
    
    public void refresh(){
	if (mainScrollPanel!=null){
	    remove(getMainScrollPanel());
	    mainScrollPanel = null;
	    bindingTable = null;
	}
	this.add(getMainScrollPanel(), BorderLayout.CENTER);
	this.add(getButtonPanel(), BorderLayout.WEST);
	BindingTableModel otm = getBindingTable().getBindingTableModel();
	TermBinding termBinding = _controller.getTermBindings().get(_terminologyId);

	if (termBinding == null || termBinding.getBindings() == null) {
	    return;
	}
	Map<String, Binding> mapBind = termBinding.getBindings();

	Set<String> gtCodes = mapBind.keySet();

	List<String> gtCodesList = new ArrayList<String>();
	gtCodesList.addAll(gtCodes);

	Collections.sort(gtCodesList);

	for (Iterator<String> iterator = gtCodesList.iterator(); iterator.hasNext();) {
	    String gtCodeString = iterator.next();
	    Binding bind = mapBind.get(gtCodeString);
	    Vector<String> v = new Vector<String>();
	    v.add(gtCodeString);
	    v.add(getCodesCommaSeperated(bind));
	    v.add(bind.getUri()!=null?bind.getUri():"");
	    otm.addRow(v);

	}
    }

    private JPanel getButtonPanel(){
	if (buttonPanel==null){
	    buttonPanel = new JPanel();
	    buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS));
	    buttonPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
	    buttonPanel.add(getAddBindingButton());
	    buttonPanel.add(getDeleteBindingButton());
	}
	return buttonPanel;
    }

    private JButton getAddBindingButton() {
	if (addTermBtn == null) {
	    addTermBtn = new JButton();
	    addTermBtn.setIcon(GDLEditorImageUtil.ADD_ICON);
	    addTermBtn.setToolTipText(GDLEditorLanguageManager.getMessage("AddBinding"));
	    addTermBtn.setContentAreaFilled(false);
	    addTermBtn.setPreferredSize(new Dimension(16,16));
	    addTermBtn.setBorderPainted(false);
	    addTermBtn.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    addTermDefinitionInModel();
		}
	    });
	}
	return addTermBtn;
    }

    private void addTermDefinitionInModel() {	
	if(validCheck(this)){
	    Vector<String> v = new Vector<String>();
	    v.add("");
	    v.add("");
	    v.add("");
	    getBindingTable().getBindingTableModel().addRow(v);	
	}
    }

    private JButton getDeleteBindingButton() {
	if (deleteBtn == null) {
	    deleteBtn = new JButton();
	    deleteBtn.setToolTipText(GDLEditorLanguageManager.getMessage("DeleteBinding"));
	    deleteBtn.setIcon(GDLEditorImageUtil.DELETE_ICON);
	    deleteBtn.setContentAreaFilled(false);
	    deleteBtn.setPreferredSize(new Dimension(16,16));
	    deleteBtn.setBorderPainted(false);
	    deleteBtn.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    deleteTermDefinitionInModel();
		}
	    });
	}
	return deleteBtn;
    }

    private void deleteTermDefinitionInModel() {
	Set<String> bindingsCodes = _controller.getTermBindings().keySet();
	if (bindingsCodes == null || bindingsCodes.size() == 0) {
	    JOptionPane.showMessageDialog(this,
		    GDLEditorLanguageManager.getMessage("ErrorMessageDeleteTermData"));
	} else {
	    BindingTableModel otm = null;
	    int selection = JOptionPane.showConfirmDialog(this,
		    GDLEditorLanguageManager.getMessage("DeleteTerminologyMessage"), 
		    GDLEditorLanguageManager.getMessage("DeleteTermPopupTitle"),
		    JOptionPane.YES_NO_OPTION);

	    if (selection == JOptionPane.YES_OPTION) {
		otm = getBindingTable().getBindingTableModel();
		int rows[] = getBindingTable()
			.getSelectedRows();
		if (otm != null) {
		    if (rows.length >= 0) {
			for (int i = rows.length - 1; i >= 0; i--) {
			    otm.removeRow(rows[i]);
			}
			getBindingTable().updateResults();
		    }
		    otm.fireTableDataChanged();
		}
	    }
	}
    }

    public boolean validCheck(BindingPanel pannel) {

	List<String> emptyCellCheck = null;
	List<String> gtdoceDuplicateCheck = null;
	if(pannel.getBindingTable().getRowCount() == 0){
	    return true;
	}
	if (pannel.getBindingTable().getCellEditor() != null) {
	    pannel.getBindingTable().getCellEditor().stopCellEditing();
	}
	gtdoceDuplicateCheck = new ArrayList<String>();
	for (int i = 0; i < pannel.getBindingTable().getRowCount(); i++) {
	    String om = pannel.getBindingTable().getValueAt(i, 0).toString();

	    if (om.trim().length() == 0) {
		JOptionPane.showMessageDialog(this,
			"Code column cannot be empty");
		return false;
	    }else {
		gtdoceDuplicateCheck.add(om);
	    }
	}

	Set<String> set = new HashSet<String>(gtdoceDuplicateCheck);
	if(set.size() < gtdoceDuplicateCheck.size()){
	    JOptionPane.showMessageDialog(this,
		    "Cannot have duplicate Codes");
	    return false;
	}


	for (int i = 0; i < pannel.getBindingTable().getRowCount(); i++) {
	    emptyCellCheck = new ArrayList<String>();
	    for (int j = 1; j < pannel.getBindingTable().getColumnCount(); j++) {
		if (pannel.getBindingTable().getColumnCount() >= j) {
		    String om = pannel.getBindingTable().getValueAt(i, j)
			    .toString();
		    if (om.trim().length() == 0) {
			emptyCellCheck.add(null);
		    } else {
			emptyCellCheck.add(om);
		    }
		}
	    }

	    int nullindex = 0;
	    for (String nullcheck : emptyCellCheck) {
		if (nullcheck == null) {
		    nullindex++;
		}
	    }

	    if (nullindex > 1) {
		JOptionPane.showMessageDialog(this,
			"Terminology code or Uri both cannot be empty values");
		return false;
	    }
	}
	return true;
    }

    private String getCodesCommaSeperated(Binding binding)
    {
	List<CodePhrase> phraselist = binding.getCodes();

	List<CodePhrase> newList = new ArrayList<CodePhrase>();
	if (phraselist!=null) {
	    boolean firstIter = true;
	    String returnString = "";

	    Set<String> codeSet = new HashSet<String>();


	    for (CodePhrase codePhrase : phraselist) {
		if(codeSet.add(codePhrase.getCodeString()))
		{
		    newList.add(codePhrase);

		}
	    }

	    for (Iterator<String> iterSet = codeSet.iterator();iterSet.hasNext();) {
		if (firstIter) {
		    returnString = iterSet.next();
		    firstIter = false;
		    continue;
		}

		returnString += " , " + iterSet.next();
	    }

	    binding.setCodes(newList);

	    return returnString;
	}
	else
	{
	    return "";
	}
    }

    public String getOwnerTabName() {
	return _terminologyId;
    }

    public void setOwnerTabName(String ownerTabName) {
	this._terminologyId = ownerTabName;
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