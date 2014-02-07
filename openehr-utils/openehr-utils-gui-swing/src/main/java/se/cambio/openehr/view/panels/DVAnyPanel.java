package se.cambio.openehr.view.panels;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;

import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JPanel;

import org.openehr.rm.datatypes.basic.DataValue;

import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.OpenEHRDataValuesUI;
import se.cambio.openehr.view.renderers.DVSelectorRenderer;
import se.cambio.openehr.view.util.DVPanelFactory;

public class DVAnyPanel extends DVGenericPanel implements DVPanelInterface {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private DVGenericPanel _dvGenericPanel;
    private JComboBox comboBox = null;
    private boolean unitsEnabled = true;

    public DVAnyPanel(String idElement, String idTemplate, boolean unitsEnabled, boolean allowNull, boolean requestFocus){
	super(idElement, idTemplate, allowNull, requestFocus);
	this.unitsEnabled = unitsEnabled;
	this.setLayout(new BorderLayout());
	JPanel panelAux = new JPanel(new FlowLayout(FlowLayout.LEFT));
	panelAux.add(getComboBox());
	this.add(panelAux, BorderLayout.EAST);
	this.add(getDVGenericPanel(), BorderLayout.CENTER);
    }

    public JComboBox getComboBox(){
	if (comboBox==null){
	    comboBox  = new JComboBox();
	    for (String idDataValue: OpenEHRDataValuesUI.getManagedDVs()){
		comboBox.addItem(idDataValue);
	    }
	    comboBox.setSelectedItem(OpenEHRDataValues.DV_TEXT);
	    comboBox.setRenderer(new DVSelectorRenderer());
	    comboBox.addActionListener(new ActionListener() {
		@Override
		public void actionPerformed(ActionEvent e) {
		    String idDataValue = (String)getComboBox().getSelectedItem();
		    updateGenericPanel(idDataValue);
		}
	    });
	}
	return comboBox;
    }

    public boolean isUnitsEnabled(){
	return unitsEnabled;
    }

    public DVGenericPanel getDVGenericPanel(){
	if (_dvGenericPanel==null){
	    _dvGenericPanel = new DVTextPanel(getIdElement(), getIdTemplate(), isAllowsNull(), isRequestFocus());
	}
	return _dvGenericPanel;
    }

    public void updateGenericPanel(String idDataValue){
	this.remove(getDVGenericPanel());
	_dvGenericPanel = DVPanelFactory.createDVPanel(getIdElement(), getIdTemplate(), idDataValue, isAllowsNull(), isUnitsEnabled(), isRequestFocus());
	this.add(getDVGenericPanel(), BorderLayout.CENTER);
	this.validate();
	this.repaint();
    }

    public void setDataValue(DataValue dataValue) {
	getDVGenericPanel().setDataValue(dataValue);
    }

    public DataValue getDataValue() {
	return getDVGenericPanel().getDataValue();
    }

    @Override
    public Collection<JComponent> getJComponents() {
	Collection<JComponent> components = new ArrayList<JComponent>();
	components.addAll(getDVGenericPanel().getJComponents());
	return components;
    }

    public static void main(String[] args){
	JDialog dialog = new JDialog();
	dialog.setSize(new Dimension(300,80));
	dialog.setContentPane(new DVAnyPanel(null, null, true, true, true));
	dialog.setVisible(true);
	//System.exit(0);
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