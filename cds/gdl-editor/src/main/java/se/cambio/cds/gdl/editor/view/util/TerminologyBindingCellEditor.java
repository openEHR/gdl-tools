package se.cambio.cds.gdl.editor.view.util;

import javax.swing.DefaultCellEditor;
import javax.swing.JTextField;
import javax.swing.event.CellEditorListener;
import javax.swing.event.ChangeEvent;

import se.cambio.cds.gdl.editor.view.tables.BindingTable;

public class TerminologyBindingCellEditor extends DefaultCellEditor {

    private BindingTable _bt = null;

    public TerminologyBindingCellEditor(BindingTable bt) {
	super(new JTextField());
	_bt = bt;
	this.addCellEditorListener(new CellEditorListener() {
	    public void editingStopped(ChangeEvent e) {
		_bt.updateResults();
	    }
	    public void editingCanceled(ChangeEvent e) {
	    }
	});
    }

    private static final long serialVersionUID = 1L;

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