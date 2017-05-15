package se.cambio.openehr.view.trees.renderers;

import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.view.trees.SelectableNode;
import se.cambio.openehr.view.trees.SelectableNodeBuilder;
import se.cambio.openehr.view.util.MultipleIcon;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeCellRenderer;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;


public class CheckBoxNodeRenderer<E> implements TreeCellRenderer {
    private JCheckBox leafRenderer = new JCheckBox();
    private SelectableNode<E> selectableNode = new <E>SelectableNodeBuilder().createSelectableNode();


    private Color selectionForeground, selectionBackground,
            textForeground, textBackground;

    public JCheckBox getLeafRenderer() {
        return leafRenderer;
    }

    public SelectableNode<?> getNodoSeleccionable() {
        return selectableNode;
    }

    public CheckBoxNodeRenderer() {
        Boolean booleanValue = (Boolean) UIManager.get("Tree.drawsFocusBorderAroundIcon");
        leafRenderer.setFocusPainted((booleanValue != null)
                && (booleanValue));
        leafRenderer.addActionListener(new CheckBoxNodeActionListener());
        selectionForeground = UIManager.getColor("Tree.selectionForeground");
        selectionBackground = UIManager.getColor("Tree.selectionBackground");
        textForeground = UIManager.getColor("Tree.textForeground");
        textBackground = UIManager.getColor("Tree.textBackground");
    }

    @SuppressWarnings("unchecked")
    public Component getTreeCellRendererComponent(JTree tree, Object value,
                                                  boolean selected, boolean expanded, boolean leaf, int row,
                                                  boolean hasFocus) {

        //Boolean visible = getNodoSeleccionable().getVisible();
        //leafRenderer.setVisible(visible);
        //if (!visible) return leafRenderer;
        if (value instanceof SelectableNode<?>){
	    /*
			Object strValue = tree.convertValueToText(value, selected,
					expanded, leaf, row, false);
	     */
            SelectableNode<?> nodo = (SelectableNode<?>)value;
            leafRenderer.setText(nodo.getName());
            leafRenderer.setToolTipText(nodo.getDescription());
            tree.setToolTipText(nodo.getDescription());
        }
        leafRenderer.setSelected(false);
        leafRenderer.setEnabled(tree.isEnabled());
        if (selected) {
            leafRenderer.setForeground(selectionForeground);
            leafRenderer.setBackground(selectionBackground);
        } else {
            leafRenderer.setForeground(textForeground);
            leafRenderer.setBackground(textBackground);
        }


        if ((value != null) && (value instanceof DefaultMutableTreeNode)) {
            if (value instanceof SelectableNode<?>) {
                SelectableNode<E> node = (SelectableNode<E>) value;
                selectableNode = node;
                leafRenderer.setText(node.getName());
                Font fontValue = UIManager.getFont("Tree.font");
                if (fontValue != null) {
                    leafRenderer.setFont(fontValue);
                }
                if (node.isItalic()) {
                    leafRenderer.setFont(leafRenderer.getFont().deriveFont(Font.ITALIC));
                }
                if (node.isBold()) {
                    leafRenderer.setFont(leafRenderer.getFont().deriveFont(Font.BOLD));
                }
                if (node.getForegroundColor() != null) {
                    leafRenderer.setForeground(node.getForegroundColor());
                }
                if (node.isMultipleSelectionMode()) {
                    Icon selectedIcon = OpenEHRImageUtil.ACCEPT_ICON;
                    Icon unSelectedIcon = OpenEHRImageUtil.UNACCEPT_ICON;
                    Icon halfSelectedIcon = OpenEHRImageUtil.HALF_ACCEPT_ICON;
                    selectedIcon = new MultipleIcon( new Icon[]{
                            selectedIcon,
                            ((SelectableNode<?>)value).getIcon()});
                    unSelectedIcon = new MultipleIcon( new Icon[]{
                            unSelectedIcon,
                            ((SelectableNode<?>)value).getIcon()});
                    halfSelectedIcon = new MultipleIcon( new Icon[]{
                            halfSelectedIcon,
                            ((SelectableNode<?>)value).getIcon()});
                    //leafRenderer.setIcon(new MultipleIcon(unSelectedicons));
                    leafRenderer.setSelectedIcon(selectedIcon);
                    leafRenderer.setDisabledSelectedIcon(unSelectedIcon);
                    if (node.isSelected()) {
                        leafRenderer.setIcon(selectedIcon);
                    } else if (node.hasChildrenSelected()) {
                        leafRenderer.setIcon(halfSelectedIcon);
                    } else {
                        leafRenderer.setIcon(unSelectedIcon);
                    }
                } else {
                    Icon icon = ((SelectableNode<?>)value).getIcon();
                    leafRenderer.setIcon(icon);
                    leafRenderer.setSelectedIcon(icon);
                    leafRenderer.setDisabledSelectedIcon(icon);
                }
                leafRenderer.setDisabledIcon(leafRenderer.getIcon());
                leafRenderer.setSelected(node.isSelected());
            }

        }
        return leafRenderer;
    }

    class CheckBoxNodeActionListener implements ActionListener {
        public void actionPerformed(ActionEvent e) {
            selectableNode.stateChange(selectableNode);
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