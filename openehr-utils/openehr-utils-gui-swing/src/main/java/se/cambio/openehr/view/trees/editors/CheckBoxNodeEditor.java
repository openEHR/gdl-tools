package se.cambio.openehr.view.trees.editors;

import se.cambio.openehr.view.trees.SelectableNode;
import se.cambio.openehr.view.trees.renderers.CheckBoxNodeRenderer;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeCellEditor;
import javax.swing.tree.TreePath;
import java.awt.*;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.util.EventObject;

public class CheckBoxNodeEditor extends AbstractCellEditor implements TreeCellEditor {

    private static final long serialVersionUID = -8830354408101728168L;

    private CheckBoxNodeRenderer<?> renderer = new CheckBoxNodeRenderer<>();

    private JTree tree;

    public CheckBoxNodeEditor(JTree tree) {
        this.tree = tree;
    }

    public Object getCellEditorValue() {
        JCheckBox checkbox = renderer.getLeafRenderer();
        renderer.getNodoSeleccionable().setAllSelected(checkbox.isSelected());
        tree.setToolTipText(checkbox.getToolTipText());
        tree.repaint();
        return checkbox;
    }

    public boolean isCellEditable(EventObject event) {
        boolean returnValue = false;
        if (event instanceof MouseEvent) {
            MouseEvent mouseEvent = (MouseEvent) event;
            TreePath path = tree.getPathForLocation(mouseEvent.getX(),
                    mouseEvent.getY());
            if (path != null) {
                Object node = path.getLastPathComponent();
                if ((node != null) && (node instanceof DefaultMutableTreeNode)) {
                    DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode) node;
                    returnValue = (treeNode instanceof SelectableNode<?>);
                }
            }
        }
        return returnValue;
    }

    public Component getTreeCellEditorComponent(JTree tree, Object value,
                                                boolean selected, boolean expanded, boolean leaf, int row) {
        Component editor =
                renderer.getTreeCellRendererComponent(
                        tree, value, true, expanded, leaf, row, true);

        ItemListener itemListener = itemEvent -> {
            if (stopCellEditing()) {
                fireEditingStopped();
            }
        };
        if (editor instanceof JCheckBox) {
            ((JCheckBox) editor).addItemListener(itemListener);
        }
        return editor;
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