package se.cambio.cds.gdl.editor.view.dialog;

import se.cambio.openehr.view.dialogs.DialogEditor;

import javax.swing.*;
import java.awt.*;
import java.util.Collection;

public class DialogComboBoxInsert extends DialogEditor {

    private static final long serialVersionUID = 2562412853124970610L;
    private JComboBox<String> comboBox = null;


    public DialogComboBoxInsert(Window owner, String title, String oldValue, Collection<String> options) {
        super(owner, title, new Dimension(250, 100), true);
        for (String option : options) {
            getComboBox().addItem(option);
        }
        if (oldValue != null) {
            getComboBox().setSelectedItem(oldValue);
        }
        initialize();
    }

    private void initialize() {
        registerComponentWithFirstFocus(getComboBox());
        getJPanel().setLayout(new BorderLayout());
        JPanel panel1 = new JPanel(new FlowLayout(FlowLayout.CENTER));
        panel1.add(new JLabel(getTitle() + ": "));
        panel1.add(getComboBox());
        getJPanel().add(panel1, BorderLayout.CENTER);
        JPanel panel2 = new JPanel(new FlowLayout(FlowLayout.CENTER));
        panel2.add(getAcceptButton());
        panel2.add(getCancelButton());
        getJPanel().add(panel2, BorderLayout.SOUTH);
    }

    public String getSelectedItem() {
        return getComboBox().getSelectedItem().toString();
    }

    private JComboBox<String> getComboBox() {
        if (comboBox == null) {
            comboBox = new JComboBox<>();
        }
        return comboBox;
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