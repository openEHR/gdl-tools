package se.cambio.openehr.view.panels;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.DvProportion;
import org.openehr.rm.datatypes.quantity.ProportionKind;
import se.cambio.openehr.controller.session.data.ProportionTypesUI;
import se.cambio.openehr.util.OpenEHRNumberFormat;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collection;

public class DVProportionPanel extends DVGenericPanel{

    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private JFormattedTextField numeratorTextField;
    private JFormattedTextField denominatorTextField;
    private JComboBox proportionTypeComboBox;
    private static int DEFAULT_PRECISION = 0;

    public DVProportionPanel(String idElement, String idTemplate, boolean allowNull, boolean requestFocus){
        super(idElement, idTemplate, allowNull, requestFocus);
        this.setLayout(new FlowLayout(FlowLayout.LEFT,0,0));
        this.add(getNumeratorTextField());
        this.add(new JLabel(" / "));
        this.add(getDenominatorTextField());
        this.add(Box.createHorizontalStrut(5));
        this.add(getProportionTypeComboBox());
    }

    protected JFormattedTextField getNumeratorTextField(){
        if (numeratorTextField==null){
            DecimalFormat format = OpenEHRNumberFormat.getDecimalFormat();
            numeratorTextField = new JFormattedTextField(format){
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
            numeratorTextField.setPreferredSize(new Dimension(100,18));
        }
        return numeratorTextField;
    }

    protected JFormattedTextField getDenominatorTextField(){
        if (denominatorTextField==null){
            DecimalFormat format = OpenEHRNumberFormat.getDecimalFormat();
            denominatorTextField = new JFormattedTextField(format){
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
            denominatorTextField.setPreferredSize(new Dimension(100,18));
            if (isRequestFocus()){
                SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        denominatorTextField.requestFocus();
                    }
                });
            }
        }
        return denominatorTextField;
    }

    protected JComboBox getProportionTypeComboBox(){
        if (proportionTypeComboBox==null){
            proportionTypeComboBox = new JComboBox();
            //proportionTypeComboBox.setPreferredSize(new Dimension(60,18));
            proportionTypeComboBox.setRenderer(new ProportionTypeComboRenderer());
            for (ProportionKind proportionKind : ProportionTypesUI.getProportionTypes(getIdTemplate(), getIdElement())) {
                proportionTypeComboBox.addItem(proportionKind);
            }
            proportionTypeComboBox.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    if (ProportionKind.UNITARY.equals(proportionTypeComboBox.getSelectedItem())){
                        getDenominatorTextField().setValue(1);
                        getDenominatorTextField().setEnabled(false);
                    }else if (ProportionKind.PERCENT.equals(proportionTypeComboBox.getSelectedItem())){
                        getDenominatorTextField().setValue(100);
                        getDenominatorTextField().setEnabled(false);
                    }else{
                        getDenominatorTextField().setEnabled(true);
                        getDenominatorTextField().setText("");
                    }
                }
            });
            if (proportionTypeComboBox.getItemCount()>0){
                proportionTypeComboBox.setSelectedIndex(0);
            }
        }
        return proportionTypeComboBox;
    }

    private class ProportionTypeComboRenderer extends JLabel implements ListCellRenderer {
        /**
         *
         */
        private static final long serialVersionUID = 1L;
        public ProportionTypeComboRenderer(){
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
            if (value instanceof ProportionKind){
                ProportionKind proportionKind = (ProportionKind) value;
                String text = ProportionTypesUI.getName(proportionKind);
                setText(text);
                setToolTipText(ProportionTypesUI.getDescription(proportionKind));
            }
            return this;
        }
    }

    private ProportionKind getSelectedType(){
        if (getProportionTypeComboBox().getItemCount()>0){
            return (ProportionKind)getProportionTypeComboBox().getSelectedItem();
        }else{
            return null;
        }
    }

    private int getPrecision(){
        String valueStr = getNumeratorTextField().getText().trim();
        int index = valueStr.lastIndexOf(".");
        if (index<=0){
            valueStr = getDenominatorTextField().getText().trim();
            index = valueStr.lastIndexOf(".");
            if (index<=0){
                return DEFAULT_PRECISION;
            }else{
                return valueStr.length()-(index+1);
            }
        }else{
            return valueStr.length()-(index+1);
        }
    }

    public void setDataValue(DataValue dataValue) {
        String numerator = "";
        String denominator = "";
        ProportionKind proportionKind = ProportionKind.FRACTION;
        if (dataValue instanceof DvProportion){
            DvProportion dvProportion = (DvProportion)dataValue;
            numerator = OpenEHRNumberFormat.roundToStr(dvProportion.getNumerator(), dvProportion.getPrecision());
            denominator = OpenEHRNumberFormat.roundToStr(dvProportion.getDenominator(), dvProportion.getPrecision());
            proportionKind = dvProportion.getType();
        }
        getProportionTypeComboBox().setSelectedItem(proportionKind);
        getNumeratorTextField().setText(numerator);
        if (getDenominatorTextField().isEnabled()){
            getDenominatorTextField().setText(denominator);
        }

    }

    public DataValue getDataValue(){
        if (!getNumeratorTextField().getText().isEmpty() &&
                !getDenominatorTextField().getText().isEmpty()){
            return new DvProportion(
                    Double.parseDouble(getNumeratorTextField().getText()),
                    Double.parseDouble(getDenominatorTextField().getText()),
                    getSelectedType(),
                    getPrecision());
        }else{
            return null;
        }
    }

    public Collection<JComponent> getJComponents() {
        Collection<JComponent> components = new ArrayList<JComponent>();
        components.add(getNumeratorTextField());
        components.add(getDenominatorTextField());
        components.add(getProportionTypeComboBox());
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