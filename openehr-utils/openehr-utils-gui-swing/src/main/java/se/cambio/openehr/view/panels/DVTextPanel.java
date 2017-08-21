package se.cambio.openehr.view.panels;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.text.DvText;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.Collection;

public class DVTextPanel extends DVGenericPanel {

    private static final long serialVersionUID = 1L;
    private JTextField valueTextField;

    public DVTextPanel(String idElement, String idTemplate, boolean allowNull, boolean requestFocus) {
        super(idElement, idTemplate, allowNull, requestFocus);
        this.setLayout(new BorderLayout());
        this.add(getValueTextField(), BorderLayout.CENTER);
    }

    protected JTextField getValueTextField() {
        if (valueTextField == null) {
            valueTextField = new JTextField();
            valueTextField.setPreferredSize(new Dimension(250, 18));
            if (isRequestFocus()) {
                SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        valueTextField.requestFocus();
                    }
                });
            }
        }
        return valueTextField;
    }

    public void setDataValue(DataValue dataValue) {
        String value = "";
        if (dataValue instanceof DvText) {
            value = ((DvText) dataValue).getValue();
        }
        getValueTextField().setText(value);
    }

    public DataValue getDataValue() {
        if (getValueTextField().getText().isEmpty()) {
            return null;
        } else {
            return new DvText(getValueTextField().getText());
        }
    }

    public Collection<JComponent> getJComponents() {
        Collection<JComponent> components = new ArrayList<JComponent>();
        components.add(getValueTextField());
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