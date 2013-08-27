
package se.cambio.cds.gdl.editor.view.util;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.view.dialog.DialogGTCodeSelection;
import se.cambio.cds.gdl.editor.view.tables.BindingTable;

public class GTCodeButtonEditor extends ButtonEditor {
    
    private static final long serialVersionUID = 4720175033111295429L;
    private BindingTable _bt = null;
    private String _gtCode = null;
    public GTCodeButtonEditor(BindingTable bt) {
	super();
	_bt = bt;
    }

    public void performAction(int row){
	GDLEditor controller = EditorManager.getActiveGDLEditor();
	DialogGTCodeSelection dialog = new DialogGTCodeSelection(EditorManager.getActiveEditorWindow(), controller);
	dialog.setVisible(true);
	if (dialog.getAnswer()){
	    _gtCode = dialog.getSelectedObject();
	    _bt.getModel().setValueAt(_gtCode, row, 0);
	    _bt.updateResults();
	}else{
	    _gtCode = (String)_bt.getModel().getValueAt(row, 0);
	}
    }

    @Override
    public Object getCellEditorValue() {
	return _gtCode;
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