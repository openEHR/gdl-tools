
package se.cambio.cds.gdl.editor.view.renderers;

import java.awt.Component;

import javax.swing.JTable;
import javax.swing.table.TableCellRenderer;

import se.cambio.cds.gdl.editor.view.panels.TerminologyCodesWithButtonPanel;

public class TerminologyCodesButtonRenderer extends TerminologyCodesWithButtonPanel implements TableCellRenderer {

    private static final long serialVersionUID = 3687161456388975032L;
 
    public TerminologyCodesButtonRenderer(){
	super();
    }

    public Component getTableCellRendererComponent(JTable table, Object value,
	    boolean isSelected, boolean hasFocus, int row, int column) {
	if (value instanceof String){
	getTextField().setText((String)value);
	}else{
	    getTextField().setText("*EMPTY*");
	}
	return this;
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