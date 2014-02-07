/*
 * Created on 26-oct-2006
 *


 */
package se.cambio.openehr.view.dialogs;

import se.cambio.openehr.view.panels.SelectionPanel;
import se.cambio.openehr.view.trees.SelectableNode;
import se.cambio.openehr.view.util.NodeConversor;

import javax.swing.*;
import javax.swing.plaf.TreeUI;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Collection;
/**
 * @author icorram
 */
public class DialogSelection extends DialogEditor {


    private static int BIG_LIST_THRESHOLD = 2000;
    /**
     *
     */
    private static final long serialVersionUID = -2770907170844293126L;
    private SelectionPanel selectionPanel = null;
    private SelectableNode<?> _rootNode = null;
    private JPanel butonsPanel = null;
    private boolean _bigList = false;
    /**
     * This is the default constructor
     */
    public DialogSelection(Window owner, String title, SelectableNode<?> rootNode, boolean expandTree, Dimension dimension) {
        super(owner, title, dimension, true, true);
        _rootNode = rootNode;
        initialize();
        if (expandTree){
            expandTree();
        }
    }
    public DialogSelection(Window owner, String titulo,SelectableNode<?> rootNode) {
        super(owner, titulo, new Dimension(300, 500), true);
        _rootNode = rootNode;
        initialize();
        expandTree();
    }

    public void setRootNode(SelectableNode<?> rootNode, boolean expandTree){
        _rootNode = rootNode;
        getJPanel().removeAll();
        selectionPanel = null;
        initialize();
        if (expandTree){
            expandTree();
        }
    }

    /**
     * This method initializes this
     */
    private void initialize() {
        if (_rootNode.getAllChildrenCount()>BIG_LIST_THRESHOLD){
            _bigList = true;
        }
        NodeConversor.setAllVisible(_rootNode);
        registerComponentWithFirstFocus(getSelectionPanel().getTextWithCleanButtonPanel().getJTextField());
        GridBagConstraints gbc = new GridBagConstraints();
        getJPanel().setLayout(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1.0;
        gbc.fill = java.awt.GridBagConstraints.BOTH;
        gbc.gridwidth = 1;
        gbc.weighty = 1;
        getJPanel().add(getSelectionPanel(), gbc);
        gbc.weighty = 0;
        gbc.gridy++;
        getJPanel().add(getButonsPanel(), gbc);
        KeyStroke enter = KeyStroke.getKeyStroke( KeyEvent.VK_ENTER,0,true);
        getJPanel().registerKeyboardAction(null, enter, JComponent.WHEN_IN_FOCUSED_WINDOW);
    }

    public JPanel getButonsPanel(){
        if (butonsPanel==null){
            butonsPanel = new JPanel();
            GridBagConstraints gbc = new GridBagConstraints();
            butonsPanel.setLayout(new GridBagLayout());
            gbc.gridwidth = 1;
            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.fill = GridBagConstraints.NONE;
            gbc.anchor = java.awt.GridBagConstraints.EAST;
            gbc.insets = new java.awt.Insets(3,10,3,5);
            butonsPanel.add(getAcceptButton(), gbc);
            gbc.anchor = java.awt.GridBagConstraints.WEST;
            gbc.gridx++;
            gbc.insets = new java.awt.Insets(3,5,3,0);
            butonsPanel.add(getCancelButton(), gbc);
        }
        return butonsPanel;
    }

    public SelectionPanel getSelectionPanel(){
        if (selectionPanel == null){
            selectionPanel = new SelectionPanel(_rootNode, true, _bigList);
            selectionPanel.getJTree().addExtraMouseListener(new DoubleClickMouseListener());
        }
        return selectionPanel;
    }


    protected boolean cancelDialog(){
        _rootNode.setAllSelected(Boolean.FALSE);
        return true;
    }

    class DoubleClickMouseListener extends MouseAdapter{
        public void mouseClicked(MouseEvent e) {
            if(e.getClickCount()>1){
                SelectableNode<?> selectableNode = NodeConversor.getSelectedNode(_rootNode, false);
                if (selectableNode!=null && selectableNode.getSeleccionUnica() && selectableNode.getObject()!=null){
                    accept();
                }
            }
        }
    }

    public Object getSelectedObject(){
        return NodeConversor.getSelectedObject(_rootNode);
    }

    public Collection<Object> getSelectedObjects(){
        return NodeConversor.getSelectedObjects(_rootNode);
    }

    public SelectableNode<?> getNode(){
        return _rootNode;
    }

    public void expandTree(){
        TreeUI ui = getSelectionPanel().getJTree().getUI();
        getSelectionPanel().getJTree().setUI(null);
        getSelectionPanel().getJTree().expand(_rootNode);
        getSelectionPanel().getJTree().setUI(ui);
    }


}  //  @jve:decl-index=0:visual-constraint="124,21"
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