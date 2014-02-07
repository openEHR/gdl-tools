package se.cambio.cds.formgen.view.panels;

import se.cambio.cds.view.swing.DVSwingUtil;
import se.cambio.openehr.view.panels.DVGenericPanel;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

public class PanelWithButtons extends JPanel {

    private static final long serialVersionUID = 1L;
    private Map<JLabel, DVGenericPanel> _dvGenericPanelsMap = null;
    private JButton _removeButton = null;
    private JButton _addButton = null;
    public PanelWithButtons(String name, Map<JLabel, DVGenericPanel> dvGenericPanelsMap, JButton addButton, JButton removeButton){
	this.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));
	_dvGenericPanelsMap = dvGenericPanelsMap;
	_addButton = addButton;
	_removeButton = removeButton;
	JPanel panelAux = new JPanel(new FlowLayout(FlowLayout.LEFT,0,0));
	panelAux.setLayout(new BoxLayout(panelAux, BoxLayout.Y_AXIS));
	this.add(panelAux);
	this.setBorder(BorderFactory.createTitledBorder(name));
	for (JLabel label : dvGenericPanelsMap.keySet()) {
	    JPanel panelAux2 = new JPanel(new FlowLayout(FlowLayout.LEFT,0,0));
	    panelAux2.add(label);
        panelAux2.add(Box.createHorizontalStrut(5));
        panelAux2.add(new JLabel("="));
        panelAux2.add(Box.createHorizontalStrut(5));
	    panelAux2.add(dvGenericPanelsMap.get(label));
	    panelAux.add(panelAux2);
	}
	if (_addButton!=null){
	    this.add(_addButton);
	}
	if (_removeButton!=null){
	    this.add(_removeButton);
	}
	this.revalidate();
	this.repaint();
    }

    private Collection<JComponent> getJComponents(){
	Collection<JComponent> components = new ArrayList<JComponent>();
	for (DVGenericPanel dvGenericPanel : _dvGenericPanelsMap.values()) {
	    components.addAll(dvGenericPanel.getJComponents());
	}
	return components;
    }

    protected void setEditableComponents(boolean editable){
	for (JComponent comp : getJComponents()) {
	    if (editable){
		DVSwingUtil.enable(comp);
		if (_addButton!=null){
		    _addButton.setVisible(true);
		}
		if (_removeButton!=null){
		    _removeButton.setVisible(true);
		}
	    }else{
		DVSwingUtil.disable(comp);
		if (_addButton!=null){
		    _addButton.setVisible(false);
		}
		if (_removeButton!=null){
		    _removeButton.setVisible(false);
		}
	    }
	}
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