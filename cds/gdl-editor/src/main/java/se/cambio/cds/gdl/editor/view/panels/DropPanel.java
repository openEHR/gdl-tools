package se.cambio.cds.gdl.editor.view.panels;
import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.JComponent;
import javax.swing.JPanel;

public class DropPanel extends JPanel {
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private GridBagConstraints _gbc = null;

    public DropPanel() {
	super();
	super.setLayout(new GridBagLayout());
    }

    public GridBagConstraints getGBC(){
	if (_gbc==null){
	    _gbc = new GridBagConstraints();
	    _gbc.weightx = 1;
	    _gbc.weighty = 1;
	    _gbc.fill = GridBagConstraints.NONE;
	    _gbc.anchor = GridBagConstraints.NORTHWEST;
	    _gbc.gridx = 0;
	    _gbc.gridy = 0;
	    _gbc.insets = new Insets(1, 0, 1, 0);
	}
	return _gbc;
    }
    /*
	public void addDraggableLine(JComponent component){
		DraggablePanel dp = new DraggablePanel(component);
		super.add(dp, getGBC());
		getGBC().gridy++;
	}
     */
    public void panelDragged(DraggablePanel panel){
	double draggedPanelY = panel.getLocation().getY();
	getGBC().gridy = 0;
	boolean inserted = false;
	for (Component component : getComponents()) {
	    DraggablePanel draggablePanel = (DraggablePanel)component;
	    double panelY = draggablePanel.getLocation().getY();
	    this.remove(component);
	    if (draggedPanelY<panelY && !inserted){
		if (component.equals(panel)){
		    this.add(panel, getGBC());
		}else{
		    this.add(panel);
		    this.add(component);
		}
		inserted = true;
	    }else{
		if (!component.equals(panel)){
		    this.add(component);
		}else{
		    continue;
		}
	    }
	}
	if (!inserted){
	    this.add(panel, getGBC());
	}
	this.repaint();
	this.validate();
    }

    public void add(JComponent c){
	this.add(c, getGBC());
	getGBC().gridy++;
    }
}/*
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