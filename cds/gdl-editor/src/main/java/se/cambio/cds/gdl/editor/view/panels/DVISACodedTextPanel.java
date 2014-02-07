package se.cambio.cds.gdl.editor.view.panels;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.openehr.rm.datatypes.text.DvText;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.openehr.view.panels.DVGenericPanel;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;

public class DVISACodedTextPanel extends DVGenericPanel{

    private static final long serialVersionUID = 1L;

    private GDLEditor _controller = null;
    private JPanel buttonPanel;
    private JComboBox comboBox = null;
    private DVGenericPanel _defaultDVGenericPanel = null;
    private DVGenericPanel _bindingDVGenericPanel = null;
    private DataValue _dataValue = null;

    public DVISACodedTextPanel(DVGenericPanel dvGenericPanel, DataValue dataValue, GDLEditor controller){
	super(dvGenericPanel.getIdElement(),
		dvGenericPanel.getIdTemplate(),
		dvGenericPanel.isAllowsNull(),
		dvGenericPanel.isRequestFocus());
	_controller = controller;
	_defaultDVGenericPanel = dvGenericPanel;
	_dataValue = dataValue;
	init();
    }

    private void init(){
	this.setLayout(new FlowLayout(FlowLayout.LEFT));
	this.add(getButtonPanel());
	this.add(getJComboBox());
	String value = null;
	if (_dataValue instanceof DvCodedText){
	    value = ((DvCodedText)_dataValue).toString();
	}else if (_dataValue instanceof DvText){
	    value = ((DvText)_dataValue).getValue();
	}
	//TODO This check should be done in a better way (avoid gt check)
	if (value!=null && value.startsWith("local::gt")){
	    getBindingDVGenericPanel().setDataValue(_dataValue);
	    getJComboBox().setSelectedIndex(1);
	}
    }

    private JPanel getButtonPanel(){
	if (buttonPanel==null){
	    buttonPanel = new JPanel();
	    buttonPanel.add(getDefaultDVGenericPanel());
	}
	return buttonPanel;
    }

    private JComboBox getJComboBox(){
	if (comboBox==null){
	    comboBox = new JComboBox();
	    comboBox.addItem(GDLEditorLanguageManager.getMessage("Default"));
	    comboBox.addItem(GDLEditorLanguageManager.getMessage("Binding"));
	    comboBox.addActionListener(new ActionListener() {
		@Override
		public void actionPerformed(ActionEvent arg0) {
		    getButtonPanel().removeAll();
		    if (getJComboBox().getSelectedIndex()==0){
			getButtonPanel().add(getDefaultDVGenericPanel());
		    }else{
			getButtonPanel().add(getBindingDVGenericPanel());
		    }
		    getButtonPanel().revalidate();
		    getButtonPanel().repaint();
		}
	    });
	}
	return comboBox;
    }

    public DVGenericPanel getDefaultDVGenericPanel(){
	return _defaultDVGenericPanel;
    }

    public DVGenericPanel getBindingDVGenericPanel(){
	if (_bindingDVGenericPanel==null){
	    _bindingDVGenericPanel = new DVLocalCodedTextPanel(_controller);
	}
	return _bindingDVGenericPanel;
    }

    public void setDataValue(DataValue dataValue) {
	getSelectedGenericPanel().setDataValue(dataValue);
    }

    public DVGenericPanel getSelectedGenericPanel(){
	return getJComboBox().getSelectedIndex()==0?getDefaultDVGenericPanel():getBindingDVGenericPanel();
    }

    public DataValue getDataValue(){
	return getSelectedGenericPanel().getDataValue();
    }

    public Collection<JComponent> getJComponents() {
	Collection<JComponent> components = new ArrayList<JComponent>();
	components.addAll(getDefaultDVGenericPanel().getJComponents());
	components.addAll(getBindingDVGenericPanel().getJComponents());
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