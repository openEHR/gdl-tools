
package se.cambio.cds.gdl.editor.view.renderers;

import java.awt.Component;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.UIManager;
import javax.swing.table.TableCellRenderer;

import org.apache.log4j.Logger;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.model.Term;

public class GTCodeButtonRenderer extends JButton implements TableCellRenderer {

    private static final long serialVersionUID = 3687161456388975032L;

    public GTCodeButtonRenderer() {
	setOpaque(true);
	setHorizontalAlignment(JLabel.LEFT);
    }

    public Component getTableCellRendererComponent(JTable table, Object value,
	    boolean isSelected, boolean hasFocus, int row, int column) {
	if (isSelected) {
	    setForeground(table.getSelectionForeground());
	    setBackground(table.getSelectionBackground());
	} else {
	    setForeground(table.getForeground());
	    setBackground(UIManager.getColor("Button.background"));
	}
	if (value==null || ((String)value).trim().isEmpty()){
	    setText(GDLEditorLanguageManager.getMessage("Select"));
	}else{
	    String gtCode = (String)value;
	    Term term = EditorManager.getActiveGDLEditor().getCurrentTermDefinition().getTerms().get(gtCode);
	    if (term!=null && term.getText()!=null){
		setText(gtCode+" - "+term.getText());
		setToolTipText(term.getDescription());
	    }else{
		setText(gtCode);
		setToolTipText("*UNKNOWN*");
		Logger.getLogger(GTCodeButtonRenderer.class).warn("GTCode '"+gtCode+"' not found!");
	    }
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