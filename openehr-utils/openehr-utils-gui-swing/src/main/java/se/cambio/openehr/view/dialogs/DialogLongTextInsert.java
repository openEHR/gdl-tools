package se.cambio.openehr.view.dialogs;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;

public class DialogLongTextInsert extends DialogEditor {

    private static final long serialVersionUID = 2562412853124970610L;
    private String oldValue = null;
    private JTextArea valueTextArea = null;
    private String _valueName = null;
    private JScrollPane jScrollPane;

    public DialogLongTextInsert(Window owner, String title, String valueName, String oldValue) {
        super(owner, title, new Dimension(400, 200), true);
        this.oldValue = oldValue;
        _valueName = valueName;
        initialize();
    }

    private void initialize() {
        registerComponentWithFirstFocus(getValueTextArea());
        getJPanel().setLayout(new BorderLayout());
        getJPanel().setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        JPanel panel1 = new JPanel(new BorderLayout());
        if (_valueName != null) {
            panel1.add(new JLabel(_valueName + ":"), BorderLayout.NORTH);
        }
        panel1.add(getJScrollPane(), BorderLayout.CENTER);
        getJPanel().add(panel1, BorderLayout.CENTER);
        JPanel panel2 = new JPanel(new FlowLayout(FlowLayout.CENTER));
        panel2.add(getAcceptButton());
        panel2.add(getCancelButton());
        getJPanel().add(panel2, BorderLayout.SOUTH);
        KeyStroke enter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, true);
        getJPanel().unregisterKeyboardAction(enter);
        this.setResizable(true);
        setVisible(true);
    }

    public String getValue() {
        return getValueTextArea().getText();
    }

    private JScrollPane getJScrollPane() {
        if (jScrollPane == null) {
            jScrollPane = new JScrollPane();
            jScrollPane.setViewportView(getValueTextArea());
        }
        return jScrollPane;
    }

    private JTextArea getValueTextArea() {
        if (valueTextArea == null) {
            valueTextArea = new JTextArea();
            valueTextArea.setBorder(BorderFactory.createEtchedBorder());
            if (oldValue != null) {
                valueTextArea.setText(oldValue);
            }
        }
        return valueTextArea;
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