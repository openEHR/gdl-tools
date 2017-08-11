package se.cambio.openehr.view.dialogs;

import se.cambio.openehr.view.panels.SelectionPanel;
import se.cambio.openehr.view.trees.SelectableNode;
import se.cambio.openehr.view.util.NodeConversor;
import se.cambio.openehr.view.util.WindowManager;

import javax.swing.*;
import javax.swing.plaf.TreeUI;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Collection;

public class DialogSelection extends DialogEditor {

    private static final int BIG_LIST_THRESHOLD = 2000;
    private static final long serialVersionUID = -2770907170844293126L;
    private SelectionPanel selectionPanel = null;
    private SelectableNode<?> rootNode = null;
    private JPanel buttonsPanel = null;
    private boolean bigList = false;
    private WindowManager windowManager;

    public DialogSelection(Window owner, String title, SelectableNode<?> rootNode, boolean expandTree, Dimension dimension, WindowManager windowManager) {
        super(owner, title, dimension, true, true);
        this.rootNode = rootNode;
        this.windowManager = windowManager;
        initialize();
        if (expandTree) {
            expandTree();
        }
    }

    public DialogSelection(Window owner, String titulo, SelectableNode<?> rootNode, WindowManager windowManager) {
        super(owner, titulo, new Dimension(300, 500), true);
        this.rootNode = rootNode;
        this.windowManager = windowManager;
        initialize();
        expandTree();
    }

    public void setRootNode(SelectableNode<?> rootNode) {
        this.rootNode = rootNode;
        getJPanel().removeAll();
        selectionPanel = null;
        initialize();
    }

    private void initialize() {
        if (rootNode.getAllChildrenCount() > BIG_LIST_THRESHOLD) {
            bigList = true;
        }
        NodeConversor.setAllVisible(rootNode);
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
        getJPanel().add(getButtonsPanel(), gbc);
        KeyStroke enter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, true);
        getJPanel().registerKeyboardAction(null, enter, JComponent.WHEN_IN_FOCUSED_WINDOW);
        this.setResizable(true);
    }

    private JPanel getButtonsPanel() {
        if (buttonsPanel == null) {
            buttonsPanel = new JPanel();
            GridBagConstraints gbc = new GridBagConstraints();
            buttonsPanel.setLayout(new GridBagLayout());
            gbc.gridwidth = 1;
            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.fill = GridBagConstraints.NONE;
            gbc.anchor = java.awt.GridBagConstraints.EAST;
            gbc.insets = new java.awt.Insets(3, 10, 3, 5);
            buttonsPanel.add(getAcceptButton(), gbc);
            gbc.anchor = java.awt.GridBagConstraints.WEST;
            gbc.gridx++;
            gbc.insets = new java.awt.Insets(3, 5, 3, 0);
            buttonsPanel.add(getCancelButton(), gbc);
        }
        return buttonsPanel;
    }

    protected SelectionPanel getSelectionPanel() {
        if (selectionPanel == null) {
            selectionPanel = new SelectionPanel(windowManager, rootNode, true, bigList);
            selectionPanel.getJTree().addExtraMouseListener(new DoubleClickMouseListener());
        }
        return selectionPanel;
    }


    protected boolean cancelDialog() {
        rootNode.setAllSelected(Boolean.FALSE);
        return true;
    }

    class DoubleClickMouseListener extends MouseAdapter {
        public void mouseClicked(MouseEvent ev) {
            if (ev.getClickCount() > 1) {
                SelectableNode<?> selectableNode = NodeConversor.getSelectedNode(rootNode, false);
                if (selectableNode != null && selectableNode.isSingleSelectionMode() && selectableNode.getObject() != null) {
                    accept();
                }
            }
        }
    }

    public Object getSelectedObject() {
        return NodeConversor.getSelectedObject(rootNode, false);
    }

    public Collection<?> getSelectedObjects() {
        return NodeConversor.getSelectedObjects(rootNode);
    }

    public SelectableNode<?> getNode() {
        return rootNode;
    }

    private void expandTree() {
        TreeUI ui = getSelectionPanel().getJTree().getUI();
        getSelectionPanel().getJTree().setUI(null);
        getSelectionPanel().getJTree().expand(rootNode);
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