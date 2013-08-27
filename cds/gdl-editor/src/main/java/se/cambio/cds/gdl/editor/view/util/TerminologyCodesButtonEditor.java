
package se.cambio.cds.gdl.editor.view.util;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.util.ArrayList;
import java.util.Collection;

import javax.swing.DefaultCellEditor;
import javax.swing.JTable;
import javax.swing.JTextField;

import org.openehr.rm.datatypes.text.CodePhrase;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.view.panels.TerminologyCodesWithButtonPanel;
import se.cambio.cds.gdl.editor.view.tables.BindingTable;
import se.cambio.openehr.view.dialogs.DialogSelection;

public class TerminologyCodesButtonEditor extends DefaultCellEditor {

    private static final long serialVersionUID = 4720175033111295429L;
    private BindingTable _bt = null;
    private TerminologyCodesWithButtonPanel _panel = null;
    private int _row = 0;

    public TerminologyCodesButtonEditor(BindingTable bt) {
	super(new JTextField());
	_bt = bt;
	_panel = new TerminologyCodesWithButtonPanel();
	_panel.getTextField().addActionListener(new ActionListener() {
	    @Override
	    public void actionPerformed(ActionEvent e) {
		_bt.getModel().setValueAt(_panel.getTextField().getText(), _row, 1);
		_bt.updateResults();
	    }
	});
	_panel.getTextField().addFocusListener(new FocusListener() {
	    @Override
	    public void focusLost(FocusEvent e) {
		update();
	    }

	    @Override
	    public void focusGained(FocusEvent e) { }
	});

	_panel.getSearchButton().addActionListener(new SearchCodesActionListener());
    }
    
    private void update(){
	_bt.getModel().setValueAt(_panel.getTextField().getText(), _row, 1);
	_bt.updateResults();
    }

    public Component getTableCellEditorComponent(JTable table, Object value,
	    boolean isSelected, int row, int column) {
	_panel.getTextField().setText((String)value);
	_row = row;
	return _panel;
    }


    private class SearchCodesActionListener implements ActionListener {
	@Override
	public void actionPerformed(ActionEvent e) {
	    String terminologyCodes = _panel.getTextField().getText();
	    Collection<String> selectedCodes = new ArrayList<String>();
	    if (!terminologyCodes.isEmpty()){
		String[] codes = terminologyCodes.split(",");
		for (String code : codes) {
		    selectedCodes.add(code.trim());
		}
	    }
	    DialogSelection dialog = 
		    new DialogSelection(
			    EditorManager.getActiveEditorWindow(),
			    _bt.getTerminologyId(),
			    NodeDefinitionConversor.getNodeAllTerminologyCodes(_bt.getTerminologyId(), selectedCodes),
			    false,
			    new Dimension(500, 600));
	    dialog.setResizable(true);
	    dialog.setVisible(true);
	    if (dialog.getAnswer()){
		StringBuffer sb = new StringBuffer();
		boolean first = true;
		for (Object object : dialog.getSelectedObjects()) {
		    if (!first){
			sb.append(", ");
		    }
		    if (object instanceof CodePhrase){
			sb.append(((CodePhrase)object).getCodeString());
			first = false;
		    }
		}
		terminologyCodes = sb.toString();
		_panel.getTextField().setText(terminologyCodes);
		update();
	    }
	}
    }
    
    @Override
    public Object getCellEditorValue() {
	return _panel.getTextField().getText();
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