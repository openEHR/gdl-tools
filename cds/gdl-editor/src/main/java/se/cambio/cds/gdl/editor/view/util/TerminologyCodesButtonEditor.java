
package se.cambio.cds.gdl.editor.view.util;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.view.panels.TerminologyCodesWithButtonPanel;
import se.cambio.cds.gdl.editor.view.tables.BindingTable;
import se.cambio.openehr.controller.sw.LoadTerminologyViewerRSW;
import se.cambio.openehr.util.TerminologyCodesManager;
import se.cambio.openehr.view.trees.SelectableNode;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.util.ArrayList;
import java.util.Collection;

public class TerminologyCodesButtonEditor extends DefaultCellEditor implements TerminologyCodesManager {

    private static final long serialVersionUID = 4720175033111295429L;
    private BindingTable bindingTable = null;
    private TerminologyCodesWithButtonPanel panel = null;
    private int row = 0;

    public TerminologyCodesButtonEditor(BindingTable bt) {
        super(new JTextField());
        bindingTable = bt;
        panel = new TerminologyCodesWithButtonPanel();
        panel.getTextField().addActionListener(e -> {
            bindingTable.getModel().setValueAt(panel.getTextField().getText(), row, 1);
            bindingTable.updateResults();
        });
        panel.getTextField().addFocusListener(new FocusListener() {
            @Override
            public void focusLost(FocusEvent e) {
                update();
            }

            @Override
            public void focusGained(FocusEvent e) { }
        });

        panel.getSearchButton().addActionListener(new SearchCodesActionListener());
    }

    public void update(){
        bindingTable.getModel().setValueAt(panel.getTextField().getText(), row, 1);
        bindingTable.updateResults();
    }

    @Override
    public void setSelectedTerminologyCodes(Collection<String> terminologyCodes) {
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        for (String terminologyCode : terminologyCodes) {
            if (!first){
                sb.append(", ");
            }else{
                first = false;
            }
            sb.append(terminologyCode);

        }
        String terminologyCodesStr = sb.toString();
        panel.getTextField().setText(terminologyCodesStr);
    }

    public Component getTableCellEditorComponent(JTable table, Object value,
                                                 boolean isSelected, int row, int column) {
        panel.getTextField().setText((String)value);
        this.row = row;
        return panel;
    }

    private TerminologyCodesManager getTerminologyCodesManager(){
        return this;
    }

    private class SearchCodesActionListener implements ActionListener {
        @Override
        public void actionPerformed(ActionEvent e) {
            String terminologyCodes = panel.getTextField().getText();
            Collection<String> selectedCodes = new ArrayList<>();
            if (!terminologyCodes.isEmpty()){
                String[] codes = terminologyCodes.split(",");
                for (String code : codes) {
                    selectedCodes.add(code.trim());
                }
            }
            new LoadTerminologyViewerRSW(
                    EditorManager.getActiveEditorWindow(),
                    getTerminologyCodesManager(),
                    bindingTable.getTerminologyId(),
                    selectedCodes,
                    SelectableNode.SelectionMode.MULTIPLE).execute();
        }
    }

    @Override
    public Object getCellEditorValue() {
        return panel.getTextField().getText();
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