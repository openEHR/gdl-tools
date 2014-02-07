package se.cambio.openehr.view.panels;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.DvCount;
import se.cambio.openehr.util.ExceptionHandler;

import javax.swing.*;
import java.awt.*;
import java.awt.event.FocusEvent;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;

public class DVCountPanel extends DVGenericPanel{

    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private JTextField valueTextField;
    public DVCountPanel(String idElement, String idTemplate, boolean allowNull, boolean requestFocus){
        super(idElement, idTemplate, allowNull, requestFocus);
        this.setLayout(new BorderLayout());
        this.add(getValueTextField(), BorderLayout.CENTER);
    }

    protected JTextField getValueTextField(){
        if (valueTextField==null){
            valueTextField = new JFormattedTextField(NumberFormat.getIntegerInstance()){
                private static final long serialVersionUID = 1L;
                @Override
                protected void processFocusEvent(final FocusEvent e) {
                    if (e.isTemporary()) {
                        return;
                    }

                    if (e.getID() == FocusEvent.FOCUS_LOST) {
                        if (getText() == null || getText().isEmpty()) {
                            setValue(null);
                        }
                    }
                    super.processFocusEvent(e);
                }
            };
            valueTextField.setPreferredSize(new Dimension(150,18));
            if (isRequestFocus()){
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
        if (dataValue instanceof DvCount){
            value = ""+((DvCount)dataValue).getMagnitude();
        }
        getValueTextField().setText(value);
    }


    public DataValue getDataValue(){
        if (getValueTextField().getText().isEmpty()){
            return null;
        }else{
            try {
                return new DvCount(NumberFormat.getInstance().parse(getValueTextField().getText()).intValue());
            } catch (ParseException e) {
                ExceptionHandler.handle(e);
                return null;
            }
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