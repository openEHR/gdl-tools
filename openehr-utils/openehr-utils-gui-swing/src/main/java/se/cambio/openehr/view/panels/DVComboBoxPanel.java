package se.cambio.openehr.view.panels;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public abstract class DVComboBoxPanel extends DVGenericPanel implements DVPanelInterface{

    private static final long serialVersionUID = 1L;
    private JComboBox comboBox;
    private Map<String, String> _itemsName = null;
    private Map<String, String> _itemsDescription = null;

    public DVComboBoxPanel(String idElement, String idTemplate, boolean allowsNull, boolean requestFocus){
        super(idElement, idTemplate, allowsNull, requestFocus);
        this.setLayout(new BorderLayout());
        _itemsName = new HashMap<String, String>();
        _itemsDescription = new HashMap<String, String>();
        this.add(getComboBox(), BorderLayout.CENTER);
    }

    protected JComboBox getComboBox(){
        if (comboBox==null){
            comboBox = new JComboBox();
            comboBox.setRenderer(new DVComboBoxRendered());
            if (isAllowsNull()){
                comboBox.addItem(" ");
            }
            if (isRequestFocus()){
                SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        comboBox.requestFocus();
                    }
                });
            }
        }
        return comboBox;
    }

    protected void insertOption(String index, String name, String description){
        getComboBox().addItem(index);
        _itemsName.put(index, name);
        _itemsDescription.put(index, description);
    }

    private class DVComboBoxRendered  extends JLabel implements ListCellRenderer{
        /**
         *
         */
        private static final long serialVersionUID = 1L;
        public DVComboBoxRendered(){
            setOpaque(true);
            setHorizontalAlignment(LEFT);
            setVerticalAlignment(CENTER);
        }


        public Component getListCellRendererComponent(JList list, Object value,
                                                      int index, boolean isSelected, boolean cellHasFocus) {
            if (isSelected) {
                setBackground(list.getSelectionBackground());
                setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
            }
            if (_itemsName.containsKey(value)){
                setText(_itemsName.get(value));
                setToolTipText(_itemsDescription.get(value));
            }else{
                setText(" ");
                setToolTipText("");
            }
            return this;
        }
    }

    public Collection<JComponent> getJComponents() {
        Collection<JComponent> components = new ArrayList<JComponent>();
        components.add(getComboBox());
        return components;
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