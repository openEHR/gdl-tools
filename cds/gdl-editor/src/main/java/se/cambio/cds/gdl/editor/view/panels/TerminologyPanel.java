package se.cambio.cds.gdl.editor.view.panels;

import org.apache.commons.jxpath.JXPathContext;
import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.panels.interfaces.RefreshablePanel;
import se.cambio.cds.gdl.editor.view.tables.TerminologyTable;
import se.cambio.cds.gdl.editor.view.tables.TerminologyTable.TerminologyTableModel;
import se.cambio.cds.gdl.model.Term;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.*;
import java.util.List;

public class TerminologyPanel extends JPanel implements RefreshablePanel{

    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private GDLEditor _controller = null;
    private JXPathContext _context = null;
    private JScrollPane terminologyScrollPanel;
    private TerminologyTable terminologyTable;
    private JButton addTermBtn = null;
    private JButton deleteBtn = null;
    private JPanel addDeleteButtonPanel;
    private JPanel mainPanel;
    private JPanel editButtonPanel;

    public TerminologyPanel(GDLEditor gdlEditor){
        _controller = gdlEditor;
        init();
    }

    public void init(){
        this.setLayout(new BorderLayout());
        this.add(getEditButtonPanel(), BorderLayout.EAST);
        this.add(getMainPanel(), BorderLayout.CENTER);
        this.setFocusable(true);
        refresh();
    }

    private JScrollPane getTerminologyScrollPanel(){
        if (terminologyScrollPanel==null){
            terminologyScrollPanel = new JScrollPane();
            terminologyScrollPanel.setViewportView(getTerminologyTable());
        }
        return terminologyScrollPanel;
    }

    private TerminologyTable getTerminologyTable(){
        if (terminologyTable==null){
            terminologyTable = new TerminologyTable(_context);
        }
        return terminologyTable;
    }

    private JPanel getMainPanel(){
        if (mainPanel==null){
            mainPanel = new JPanel(new BorderLayout());
        }
        return mainPanel;
    }

    public void refresh(){
        getMainPanel().removeAll();
        terminologyScrollPanel = null;
        terminologyTable = null;
        _context = JXPathContext.newContext(_controller.getCurrentTermsMap());
        getMainPanel().add(getTerminologyScrollPanel(), BorderLayout.CENTER);
        getMainPanel().add(getAddDeleteButtonPanel(), BorderLayout.WEST);
        TerminologyTableModel ttm = getTerminologyTable().getTerminologyTableModel();
        Map<String, Term> termMap = _controller.getCurrentTermsMap();
        List<String> gtCodes =
                new ArrayList<String>(termMap.keySet());
        Collections.sort(gtCodes);
        for (String gtCode : gtCodes) {
            Term term = termMap.get(gtCode);
            Vector<String> v = new Vector<String>();
            v.add(term.getId());
            v.add(term.getText());
            v.add(term.getDescription());
            ttm.addRow(v);
        }
        getMainPanel().revalidate();
        getMainPanel().repaint();
    }

    private JPanel getAddDeleteButtonPanel(){
        if (addDeleteButtonPanel==null){
            addDeleteButtonPanel = new JPanel();
            addDeleteButtonPanel.setLayout(new BoxLayout(addDeleteButtonPanel, BoxLayout.Y_AXIS));
            addDeleteButtonPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
            addDeleteButtonPanel.add(getAddBindingButton());
            addDeleteButtonPanel.add(getDeleteBindingButton());
        }
        return addDeleteButtonPanel;
    }

    private JButton getAddBindingButton() {
        if (addTermBtn == null) {
            addTermBtn = new JButton();
            addTermBtn.setIcon(GDLEditorImageUtil.ADD_ICON);
            addTermBtn.setToolTipText(GDLEditorLanguageManager.getMessage("AddLocalTerm"));
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
        Vector<String> v = new Vector<String>();
        v.add(_controller.createNextGTCode());
        v.add("");
        v.add("");
        getTerminologyTable().getTerminologyTableModel().addRow(v);
    }

    private JButton getDeleteBindingButton() {
        if (deleteBtn == null) {
            deleteBtn = new JButton();
            deleteBtn.setToolTipText(GDLEditorLanguageManager.getMessage("DeleteLocalTerm"));
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
        TerminologyTableModel ttm = null;
        //Look for referenced codes
        Collection<String> gtCodesUsed = new ArrayList<String>();
        gtCodesUsed.addAll(_controller.getGTCodesUsedInDefinitions());
        gtCodesUsed.addAll(_controller.getGTCodesUsedInBindings());
        for (String gtCode : getSelectedGTCodes()) {
            if (gtCodesUsed.contains(gtCode)){
                JOptionPane.showMessageDialog(
                        EditorManager.getActiveEditorWindow(),
                        GDLEditorLanguageManager.getMessage("ReferenceBeingUsedMsg"),
                        GDLEditorLanguageManager.getMessage("ReferenceBeingUsedTitle"),
                        JOptionPane.WARNING_MESSAGE);
                return;
            }
        }

        int selection = JOptionPane.showConfirmDialog(this,
                GDLEditorLanguageManager.getMessage("DeleteTermDesc"),
                GDLEditorLanguageManager.getMessage("DeleteLocalTerm"),
                JOptionPane.YES_NO_OPTION);

        if (selection == JOptionPane.YES_OPTION) {
            ttm = getTerminologyTable().getTerminologyTableModel();
            int rows[] = getTerminologyTable().getSelectedRows();
            if (ttm != null) {
                if (rows.length >= 0) {
                    for (int i = rows.length - 1; i >= 0; i--) {
                        ttm.removeRow(rows[i]);
                    }
                }
                ttm.fireTableDataChanged();
                updateResults();
            }
        }
    }

    public Collection<String> getSelectedGTCodes(){
        Collection<String> gtCodes = new ArrayList<String>();
        TerminologyTableModel ttm = getTerminologyTable().getTerminologyTableModel();
        int rows[] = getTerminologyTable().getSelectedRows();
        if (ttm != null) {
            if (rows.length >= 0) {
                for (int i = rows.length - 1; i >= 0; i--) {
                    gtCodes.add((String)ttm.getValueAt(rows[i], 0));
                }
            }
        }
        return gtCodes;
    }

    private void updateResults() {
        _controller.getCurrentTermsMap().clear();
        int numRows = getTerminologyTable().getRowCount();
        for (int i = 0; i < numRows; i++) {
            Term term = new Term();
            String gtCode = (String)getTerminologyTable().getValueAt(i, 0);
            String text = (String)getTerminologyTable().getValueAt(i, 1);
            String desc = (String)getTerminologyTable().getValueAt(i, 2);
            term.setId(gtCode);
            term.setText(text);
            term.setDescription(desc);
            _controller.getCurrentTermsMap().put(gtCode, term);
        }
    }

    private JPanel getEditButtonPanel() {
        if (editButtonPanel == null) {
            editButtonPanel = new JPanel();
            editButtonPanel.setLayout(new BoxLayout(editButtonPanel, BoxLayout.Y_AXIS));
            editButtonPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        }
        return editButtonPanel;
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