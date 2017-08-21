package se.cambio.openehr.view.panels;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.DvCount;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.awt.event.FocusEvent;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;

public class DVCountPanel extends DVGenericPanel {

    private static final long serialVersionUID = 1L;
    private JTextField valueTextField;
    private Logger logger = LoggerFactory.getLogger(DVCountPanel.class);

    public DVCountPanel(String idElement, String idTemplate, boolean allowNull, boolean requestFocus) {
        super(idElement, idTemplate, allowNull, requestFocus);
        this.setLayout(new BorderLayout());
        this.add(getValueTextField(), BorderLayout.CENTER);
    }

    private JTextField getValueTextField() {
        if (valueTextField == null) {
            valueTextField = new JFormattedTextField(NumberFormat.getIntegerInstance()) {
                private static final long serialVersionUID = 1L;

                @Override
                protected void processFocusEvent(final FocusEvent ev) {
                    if (ev.isTemporary()) {
                        return;
                    }

                    if (ev.getID() == FocusEvent.FOCUS_LOST) {
                        if (getText() == null || getText().isEmpty()) {
                            setValue(null);
                        }
                    }
                    super.processFocusEvent(ev);
                }
            };
            valueTextField.setPreferredSize(new Dimension(150, 18));
            if (isRequestFocus()) {
                SwingUtilities.invokeLater(() -> valueTextField.requestFocus());
            }
        }
        return valueTextField;
    }


    public void setDataValue(DataValue dataValue) {
        String value = "";
        if (dataValue instanceof DvCount) {
            value = "" + ((DvCount) dataValue).getMagnitude();
        }
        getValueTextField().setText(value);
    }


    public DataValue getDataValue() {
        if (getValueTextField().getText().isEmpty()) {
            return null;
        } else {
            try {
                return new DvCount(NumberFormat.getInstance().parse(getValueTextField().getText()).intValue());
            } catch (ParseException ex) {
                logger.error("Error parsing count: " + getValueTextField().getText(), ex);
                return null;
            }
        }
    }

    public Collection<JComponent> getJComponents() {
        Collection<JComponent> components = new ArrayList<>();
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