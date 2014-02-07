
package se.cambio.cds.gdl.editor.view.util;

import se.cambio.cds.gdl.editor.controller.interfaces.TerminologyCodesManager;
import se.cambio.cds.gdl.editor.controller.sw.LoadTerminologyViewerRSW;
import se.cambio.cds.gdl.editor.view.panels.TerminologyCodesWithButtonPanel;
import se.cambio.cds.gdl.editor.view.tables.BindingTable;

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

    public void update(){
        _bt.getModel().setValueAt(_panel.getTextField().getText(), _row, 1);
        _bt.updateResults();
    }

    @Override
    public void setSelectedTerminologyCodes(Collection<String> terminologyCodes) {
        StringBuffer sb = new StringBuffer();
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
        _panel.getTextField().setText(terminologyCodesStr);
    }

    public Component getTableCellEditorComponent(JTable table, Object value,
                                                 boolean isSelected, int row, int column) {
        _panel.getTextField().setText((String)value);
        _row = row;
        return _panel;
    }

    public TerminologyCodesManager getTerminologyCodesManager(){
        return this;
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
            new LoadTerminologyViewerRSW(getTerminologyCodesManager(), _bt.getTerminologyId(), selectedCodes).execute();
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