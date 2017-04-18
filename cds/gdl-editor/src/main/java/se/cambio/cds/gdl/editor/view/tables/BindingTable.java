package se.cambio.cds.gdl.editor.view.tables;

import org.openehr.rm.datatypes.text.CodePhrase;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.renderers.GTCodeButtonRenderer;
import se.cambio.cds.gdl.editor.view.renderers.TerminologyCodesButtonRenderer;
import se.cambio.cds.gdl.editor.view.util.GTCodeButtonEditor;
import se.cambio.cds.gdl.editor.view.util.TerminologyBindingCellEditor;
import se.cambio.cds.gdl.editor.view.util.TerminologyCodesButtonEditor;
import se.cambio.cds.gdl.model.Binding;
import se.cambio.openehr.util.TerminologyDialogManager;
import se.cambio.openehr.view.util.WindowManager;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Vector;

public class BindingTable extends JTable {

    private static final long serialVersionUID = 1L;
    private Map<String, Binding> bindings = null;
    private String terminologyId = null;

    public BindingTable(
            WindowManager windowManager,
            TerminologyDialogManager terminologyDialogManager,
            Map<String, Binding> bindings,
            String terminologyId,
            GDLEditor gdlEditor) {
        this.terminologyId = terminologyId;
        this.bindings = bindings;
        this.setModel(new BindingTableModel());
        Vector<String> columnIdentifiers = new Vector<>();
        columnIdentifiers.add(GDLEditorLanguageManager.getMessage("LocalTerms"));
        columnIdentifiers.add(GDLEditorLanguageManager.getMessage("TerminologyCodes"));
        columnIdentifiers.add(GDLEditorLanguageManager.getMessage("Uri"));
        getBindingTableModel().setColumnIdentifiers(columnIdentifiers);
        this.getColumnModel().getColumn(0).setPreferredWidth(200);
        this.getColumnModel().getColumn(1).setPreferredWidth(400);
        this.putClientProperty("terminateEditOnFocusLost", true);
        TerminologyBindingCellEditor cellEditor =
                new TerminologyBindingCellEditor(this);
        this.getColumnModel().getColumn(0).setCellEditor(new GTCodeButtonEditor(this, gdlEditor));
        this.getColumnModel().getColumn(0).setCellRenderer(new GTCodeButtonRenderer(gdlEditor));
        this.getColumnModel().getColumn(1).setCellEditor(new TerminologyCodesButtonEditor(windowManager, terminologyDialogManager, this));
        this.getColumnModel().getColumn(1).setCellRenderer(new TerminologyCodesButtonRenderer());
        this.getColumnModel().getColumn(2).setCellEditor(cellEditor);
    }

    public BindingTableModel getBindingTableModel() {
        return (BindingTableModel) getModel();
    }

    public boolean isCellEditable(int row, int column) {
        return true;
    }

    public class BindingTableModel extends DefaultTableModel {
        private static final long serialVersionUID = 1L;
    }


    public void updateResults() {
        bindings.clear();
        int numRows = getRowCount();
        for (int i = 0; i < numRows; i++) {
            Binding binding = new Binding();
            String gtCode = (String) getBindingTableModel().getValueAt(i, 0);
            String codes = (String) getBindingTableModel().getValueAt(i, 1);
            String uri = (String) getBindingTableModel().getValueAt(i, 2);
            binding.setId(gtCode);
            binding.setCodes(getCodePhrases(codes));
            binding.setUri(uri);
            bindings.put(gtCode, binding);
        }
    }

    public String getTerminologyId() {
        return terminologyId;
    }

    private List<CodePhrase> getCodePhrases(String value) {
        List<CodePhrase> codePhrases = new ArrayList<>();
        String[] multipleDataCodePhrase = value.split(",");
        for (String aMultipleDataCodePhrase : multipleDataCodePhrase) {
            if (!aMultipleDataCodePhrase.trim().isEmpty()) {
                CodePhrase phrase = new CodePhrase(
                        terminologyId,
                        aMultipleDataCodePhrase.trim());
                codePhrases.add(phrase);
            }
        }
        return codePhrases;
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