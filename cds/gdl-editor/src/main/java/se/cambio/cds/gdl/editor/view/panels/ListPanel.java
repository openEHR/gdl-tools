package se.cambio.cds.gdl.editor.view.panels;

import org.apache.commons.jxpath.JXPathContext;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.dialog.DialogNameInsert;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;

class ListPanel extends JPanel {

    private static final long serialVersionUID = 1L;
    private final Window window;
    private String title;
    private String xpath;
    private JXPathContext context;
    private JList<String> list;

    ListPanel(String title, String xpath, JXPathContext context, Window window) {
        this.title = title;
        this.xpath = xpath;
        this.context = context;
        this.window = window;
        Object obj = context.getValue(xpath);
        if (obj instanceof List) {
            DefaultListModel<String> dlm = ((DefaultListModel<String>) getJList().getModel());
            for (Object objAux : (List<?>) obj) {
                String value = (String) objAux;
                if (value != null) {
                    value = value.replace("\\\"", "\"");
                }
                dlm.addElement(value);
            }
        }
        init();
    }

    private void init() {
        this.setLayout(new BorderLayout());
        this.setBorder(BorderFactory.createTitledBorder(title));
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS));
        JButton addButton = generateAddButton();
        buttonPanel.add(addButton);
        JButton deleteButton = generateDeleteButton();
        buttonPanel.add(deleteButton);
        JButton editButton = generateEditButton();
        buttonPanel.add(editButton);
        this.add(buttonPanel, BorderLayout.WEST);
        this.add(getJList(), BorderLayout.CENTER);
    }

    private JButton generateAddButton() {
        JButton addButton = new JButton(GDLEditorImageUtil.ADD_ICON);
        addButton.setContentAreaFilled(false);
        addButton.setPreferredSize(new Dimension(16, 16));
        addButton.setBorderPainted(false);
        addButton.addActionListener(e -> {
            String value = JOptionPane.showInputDialog(window, title, "");
            if (value != null) {
                DefaultListModel<String> dlm = ((DefaultListModel<String>) getJList().getModel());
                dlm.addElement(value);
                updateListModel(dlm);
            }
        });
        return addButton;
    }

    private JButton generateDeleteButton() {
        JButton deleteButton = new JButton(GDLEditorImageUtil.DELETE_ICON);
        deleteButton.setContentAreaFilled(false);
        deleteButton.setPreferredSize(new Dimension(16, 16));
        deleteButton.setBorderPainted(false);
        deleteButton.addActionListener(e -> {
            int index = getJList().getSelectedIndex();
            if (index >= 0) {
                DefaultListModel dlm = ((DefaultListModel) getJList().getModel());
                dlm.removeElementAt(index);
                updateListModel(dlm);
            }
        });
        return deleteButton;
    }

    private JButton generateEditButton() {
        JButton editButton = new JButton(GDLEditorImageUtil.EDIT_ICON);
        editButton.setContentAreaFilled(false);
        editButton.setPreferredSize(new Dimension(16, 16));
        editButton.setBorderPainted(false);
        editButton.setToolTipText(GDLEditorLanguageManager.getMessage("EditKeyword"));
        editButton.addActionListener(e -> {
            int index = getJList().getSelectedIndex();
            if (index >= 0) {
                editItem(index);
            }
        });
        return editButton;
    }

    private void editItem(int index) {
        DefaultListModel<String> dlm = ((DefaultListModel<String>) getJList().getModel());
        String label = dlm.getElementAt(index);
        DialogNameInsert dialogNameInsert = new DialogNameInsert(window, GDLEditorLanguageManager.getMessage("EditKeyword"), label);
        if (dialogNameInsert.getAnswer()) {
            label = dialogNameInsert.getValue();
            dlm.setElementAt(label, index);
            updateListModel(dlm);
        }
    }

    private JList<String> getJList() {
        if (list == null) {
            list = new JList<>(new DefaultListModel<String>());
            list.setBorder(BorderFactory.createEtchedBorder());
            list.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent ev) {
                    if (ev.getClickCount() >= 2) {
                        editItem(getJList().getSelectedIndex());
                    }
                }
            });
        }
        return list;
    }

    private void updateListModel(DefaultListModel dlm) {
        List<String> elements = new ArrayList<>();
        for (int i = 0; i < dlm.getSize(); i++) {
            String value = (String) dlm.getElementAt(i);
            if (value != null) {
                value = value.replace("\"", "\\\"");
            }
            elements.add(value);
        }
        context.setValue(xpath, elements);
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