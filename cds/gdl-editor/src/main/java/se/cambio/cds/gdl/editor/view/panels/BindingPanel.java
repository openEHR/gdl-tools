package se.cambio.cds.gdl.editor.view.panels;

import org.openehr.rm.datatypes.text.CodePhrase;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.tables.BindingTable;
import se.cambio.cds.gdl.editor.view.tables.BindingTable.BindingTableModel;
import se.cambio.cds.gdl.model.Binding;
import se.cambio.cds.gdl.model.TermBinding;
import se.cambio.cds.view.swing.panel.interfaces.RefreshablePanel;

import javax.swing.*;
import java.awt.*;
import java.util.*;
import java.util.List;

public class BindingPanel extends JPanel implements RefreshablePanel {

    private static final long serialVersionUID = 1L;
    private GDLEditor controller = null;
    private JScrollPane mainScrollPanel;
    private BindingTable bindingTable;
    private String terminologyId = null;
    private JButton addTermBtn = null;
    private JButton deleteBtn = null;
    private JPanel buttonPanel;

    BindingPanel(GDLEditor gdlEditor, String terminologyId) {
        controller = gdlEditor;
        this.terminologyId = terminologyId;
        init();
    }

    public void init() {
        this.setLayout(new BorderLayout());
        refresh();
    }

    private JScrollPane getMainScrollPanel() {
        if (mainScrollPanel == null) {
            mainScrollPanel = new JScrollPane();
            mainScrollPanel.setViewportView(getBindingTable());
        }
        return mainScrollPanel;
    }

    private BindingTable getBindingTable() {
        if (bindingTable == null) {
            bindingTable =
                    new BindingTable(
                            controller.getWindowManager(),
                            controller.getTerminologyDialogManager(), controller.getTermBindings().get(terminologyId).getBindings(),
                            terminologyId,
                            controller);
        }
        return bindingTable;
    }

    public void refresh() {
        if (mainScrollPanel != null) {
            remove(getMainScrollPanel());
            mainScrollPanel = null;
            bindingTable = null;
        }
        this.add(getMainScrollPanel(), BorderLayout.CENTER);
        this.add(getButtonPanel(), BorderLayout.WEST);
        BindingTableModel otm = getBindingTable().getBindingTableModel();
        TermBinding termBinding = controller.getTermBindings().get(terminologyId);

        if (termBinding == null || termBinding.getBindings() == null) {
            return;
        }
        Map<String, Binding> mapBind = termBinding.getBindings();

        Set<String> gtCodes = mapBind.keySet();

        List<String> gtCodesList = new ArrayList<>();
        gtCodesList.addAll(gtCodes);

        Collections.sort(gtCodesList);

        for (String gtCodeString : gtCodesList) {
            Binding bind = mapBind.get(gtCodeString);
            Vector<String> vector = new Vector<>();
            vector.add(gtCodeString);
            vector.add(getCodesCommaSeperated(bind));
            vector.add(bind.getUri() != null ? bind.getUri() : "");
            otm.addRow(vector);

        }
    }

    private JPanel getButtonPanel() {
        if (buttonPanel == null) {
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
            addTermBtn.setPreferredSize(new Dimension(16, 16));
            addTermBtn.setBorderPainted(false);
            addTermBtn.addActionListener(e -> addTermDefinitionInModel());
        }
        return addTermBtn;
    }

    private void addTermDefinitionInModel() {
        if (validCheck(this)) {
            Vector<String> vector = new Vector<>();
            vector.add("");
            vector.add("");
            vector.add("");
            getBindingTable().getBindingTableModel().addRow(vector);
        }
    }

    private JButton getDeleteBindingButton() {
        if (deleteBtn == null) {
            deleteBtn = new JButton();
            deleteBtn.setToolTipText(GDLEditorLanguageManager.getMessage("DeleteBinding"));
            deleteBtn.setIcon(GDLEditorImageUtil.DELETE_ICON);
            deleteBtn.setContentAreaFilled(false);
            deleteBtn.setPreferredSize(new Dimension(16, 16));
            deleteBtn.setBorderPainted(false);
            deleteBtn.addActionListener(e -> deleteTermDefinitionInModel());
        }
        return deleteBtn;
    }

    private void deleteTermDefinitionInModel() {
        Set<String> bindingsCodes = controller.getTermBindings().keySet();
        if (bindingsCodes.size() == 0) {
            JOptionPane.showMessageDialog(this,
                    GDLEditorLanguageManager.getMessage("ErrorMessageDeleteTermData"));
        } else {
            BindingTableModel otm;
            int selection = JOptionPane.showConfirmDialog(this,
                    GDLEditorLanguageManager.getMessage("DeleteTerminologyMessage"),
                    GDLEditorLanguageManager.getMessage("DeleteTermPopupTitle"),
                    JOptionPane.YES_NO_OPTION);

            if (selection == JOptionPane.YES_OPTION) {
                otm = getBindingTable().getBindingTableModel();
                int[] rows = getBindingTable().getSelectedRows();
                if (otm != null) {
                    if (rows.length > 0) {
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

    private boolean validCheck(BindingPanel pannel) {

        List<String> gtdoceDuplicateCheck;
        if (pannel.getBindingTable().getRowCount() == 0) {
            return true;
        }
        if (pannel.getBindingTable().getCellEditor() != null) {
            pannel.getBindingTable().getCellEditor().stopCellEditing();
        }
        gtdoceDuplicateCheck = new ArrayList<>();
        for (int i = 0; i < pannel.getBindingTable().getRowCount(); i++) {
            String om = pannel.getBindingTable().getValueAt(i, 0).toString();

            if (!om.isEmpty()) {
                gtdoceDuplicateCheck.add(om);
            }
        }

        Set<String> set = new HashSet<>(gtdoceDuplicateCheck);
        if (set.size() < gtdoceDuplicateCheck.size()) {
            JOptionPane.showMessageDialog(this,
                    "Cannot have duplicate Codes");
            return false;
        }
        return true;
    }

    private String getCodesCommaSeperated(Binding binding) {
        List<CodePhrase> phraselist = binding.getCodes();

        List<CodePhrase> newList = new ArrayList<>();
        if (phraselist != null) {
            boolean firstIter = true;
            String returnString = "";

            Set<String> codeSet = new HashSet<>();


            for (CodePhrase codePhrase : phraselist) {
                if (codeSet.add(codePhrase.getCodeString())) {
                    newList.add(codePhrase);

                }
            }

            for (Iterator<String> iterSet = codeSet.iterator(); iterSet.hasNext(); ) {
                if (firstIter) {
                    returnString = iterSet.next();
                    firstIter = false;
                    continue;
                }

                returnString += " , " + iterSet.next();
            }

            binding.setCodes(newList);

            return returnString;
        } else {
            return "";
        }
    }

    String getOwnerTabName() {
        return terminologyId;
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