package se.cambio.cds.gdl.editor.view.panels;

import org.apache.commons.jxpath.JXPathContext;
import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

public class ListPanel extends JPanel{

    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private String _title;
    private String _xPath;
    private JXPathContext _context;
    private JList jList;

    public ListPanel(String title, String xPath, JXPathContext context){
        _title = title;
        _xPath = xPath;
        _context = context;
        Object obj = context.getValue(xPath);
        if (obj instanceof List){
            DefaultListModel dlm = ((DefaultListModel)getJList().getModel());
            for (Object objAux : (List<?>)obj) {
                String value = (String)objAux;
                if (value!=null){
                    value = value.replace("\\\"","\"");
                }
                dlm.addElement(value);
            }
        }
        init();
    }

    private void init(){
        this.setLayout(new BorderLayout());
        this.setBorder(BorderFactory.createTitledBorder(_title));
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS));
        JButton addButton = new JButton(GDLEditorImageUtil.ADD_ICON);
        addButton.setContentAreaFilled(false);
        addButton.setPreferredSize(new Dimension(16,16));
        addButton.setBorderPainted(false);
        addButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String value = JOptionPane.showInputDialog(EditorManager.getActiveEditorWindow(), _title, "");
                if(value!=null){
                    DefaultListModel dlm = ((DefaultListModel)getJList().getModel());
                    dlm.addElement(value);
                    updateListModel(dlm);
                }
            }
        });
        JButton deleteButton = new JButton(GDLEditorImageUtil.DELETE_ICON);
        deleteButton.setContentAreaFilled(false);
        deleteButton.setPreferredSize(new Dimension(16,16));
        deleteButton.setBorderPainted(false);
        deleteButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                int index = getJList().getSelectedIndex();
                if(index>=0){
                    DefaultListModel dlm = ((DefaultListModel)getJList().getModel());
                    dlm.removeElementAt(index);
                    updateListModel(dlm);
                }
            }
        });
        buttonPanel.add(addButton);
        buttonPanel.add(deleteButton);
        this.add(buttonPanel, BorderLayout.WEST);
        this.add(getJList(), BorderLayout.CENTER);
    }

    private JList getJList(){
        if(jList==null){
            jList = new JList(new DefaultListModel());
            jList.setBorder(BorderFactory.createEtchedBorder());
        }
        return jList;
    }

    private void updateListModel(DefaultListModel dlm){
        List<String> elements = new ArrayList<String>();
        for (int i=0; i<dlm.getSize(); i++) {
            String value = (String)dlm.getElementAt(i);
            if (value!=null){
                value = value.replace("\"","\\\"");
            }
            elements.add(value);
        }
        _context.setValue(_xPath, elements);
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