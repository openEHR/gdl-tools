package se.cambio.cds.gdl.editor.view.panels;

import javax.swing.*;
import java.awt.*;

public class DropPanel extends JPanel {
    private static final long serialVersionUID = 1L;
    private GridBagConstraints gridBagConstraints = null;

    DropPanel() {
        super();
        super.setLayout(new GridBagLayout());
    }

    GridBagConstraints getGBC() {
        if (gridBagConstraints == null) {
            gridBagConstraints = new GridBagConstraints();
            gridBagConstraints.weightx = 1;
            gridBagConstraints.weighty = 1;
            gridBagConstraints.fill = GridBagConstraints.NONE;
            gridBagConstraints.anchor = GridBagConstraints.NORTHWEST;
            gridBagConstraints.gridx = 0;
            gridBagConstraints.gridy = 0;
            gridBagConstraints.insets = new Insets(1, 0, 1, 0);
        }
        return gridBagConstraints;
    }

    public void panelDragged(DraggablePanel panel) {
        double draggedPanelY = panel.getLocation().getY();
        getGBC().gridy = 0;
        boolean inserted = false;
        for (Component component : getComponents()) {
            DraggablePanel draggablePanel = (DraggablePanel) component;
            double panelY = draggablePanel.getLocation().getY();
            this.remove(component);
            if (draggedPanelY < panelY && !inserted) {
                if (component.equals(panel)) {
                    this.add(panel, getGBC());
                } else {
                    this.add(panel);
                    this.add(component);
                }
                inserted = true;
            } else {
                if (!component.equals(panel)) {
                    this.add(component);
                }
            }
        }
        if (!inserted) {
            this.add(panel, getGBC());
        }
        this.repaint();
        this.validate();
    }

    public void add(JComponent component) {
        this.add(component, getGBC());
        getGBC().gridy++;
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