/*
 * Creado el 11-dic-2007
 *


 */
package se.cambio.openehr.view.trees.renderers;

import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.view.trees.SelectableNode;
import se.cambio.openehr.view.trees.SelectableNodeWithIcon;
import se.cambio.openehr.view.util.MultipleIcon;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeCellRenderer;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * @author icorram
 *


 */
public class CheckBoxNodeRenderer<E> implements TreeCellRenderer {
    private JCheckBox leafRenderer = new JCheckBox();
    private SelectableNode<E> _nodoSeleccionable = new SelectableNode<E>();


    Color selectionBorderColor, selectionForeground, selectionBackground,
            textForeground, textBackground;

    public JCheckBox getLeafRenderer() {
        return leafRenderer;
    }

    public SelectableNode<?> getNodoSeleccionable() {
        return _nodoSeleccionable;
    }

    public CheckBoxNodeRenderer() {
        Boolean booleanValue = (Boolean) UIManager.get("Tree.drawsFocusBorderAroundIcon");
        leafRenderer.setFocusPainted((booleanValue != null)
                && (booleanValue.booleanValue()));
        leafRenderer.addActionListener(new CheckBoxNodeActionListener());
        selectionBorderColor = UIManager.getColor("Tree.selectionBorderColor");
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
            leafRenderer.setText(nodo.getDescripcion());
            leafRenderer.setToolTipText(nodo.getToolTip());
            tree.setToolTipText(nodo.getToolTip());
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
                _nodoSeleccionable = node;
                leafRenderer.setText(node.getDescripcion());
                Font fontValue = UIManager.getFont("Tree.font");
                if (fontValue != null) {
                    leafRenderer.setFont(fontValue);
                }
                if (node.isCursiva()){
                    leafRenderer.setFont(leafRenderer.getFont().deriveFont(Font.ITALIC));
                }
                if (node.isBold()){
                    leafRenderer.setFont(leafRenderer.getFont().deriveFont(Font.BOLD));
                }
                if (node.getForeground()!=null){
                    leafRenderer.setForeground(node.getForeground());
                }
                if (!node.getSeleccionUnica()){
                    Icon selectedIcon = OpenEHRImageUtil.ACCEPT_ICON;
                    Icon unSelectedIcon = OpenEHRImageUtil.UNACCEPT_ICON;
                    Icon halfSelectedIcon = OpenEHRImageUtil.HALF_ACCEPT_ICON;
                    if (value instanceof SelectableNodeWithIcon<?>){
                        selectedIcon = new MultipleIcon( new Icon[]{
                                selectedIcon,
                                ((SelectableNodeWithIcon<?>)value).getIcono()});
                        unSelectedIcon = new MultipleIcon( new Icon[]{
                                unSelectedIcon,
                                ((SelectableNodeWithIcon<?>)value).getIcono()});
                        halfSelectedIcon = new MultipleIcon( new Icon[]{
                                halfSelectedIcon,
                                ((SelectableNodeWithIcon<?>)value).getIcono()});
                    }
                    //leafRenderer.setIcon(new MultipleIcon(unSelectedicons));
                    leafRenderer.setSelectedIcon(selectedIcon);
                    leafRenderer.setDisabledSelectedIcon(unSelectedIcon);
                    if (node.getSeleccionado().booleanValue()){
                        leafRenderer.setIcon(selectedIcon);
                    } else if (node.getContineneSeleccionado()){
                        leafRenderer.setIcon(halfSelectedIcon);
                    }else{
                        leafRenderer.setIcon(unSelectedIcon);
                    }
                }else{
                    Icon icono = OpenEHRImageUtil.EMPTY_ICON;
                    if (value instanceof SelectableNodeWithIcon<?>){
                        icono = ((SelectableNodeWithIcon<?>)value).getIcono();
                    }
                    leafRenderer.setIcon(icono);
                    leafRenderer.setSelectedIcon(icono);
                    leafRenderer.setDisabledSelectedIcon(icono);
                }
                leafRenderer.setDisabledIcon(leafRenderer.getIcon());
                leafRenderer.setSelected(node.getSeleccionado().booleanValue());
            }

        }
        return leafRenderer;
    }

    class CheckBoxNodeActionListener implements ActionListener{

        public void actionPerformed(ActionEvent e) {
            _nodoSeleccionable.cambioEstado(_nodoSeleccionable);
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