package se.cambio.openehr.view.trees;

import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Enumeration;

import javax.swing.JTree;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import se.cambio.openehr.view.trees.editors.CheckBoxNodeEditor;
import se.cambio.openehr.view.trees.renderers.CheckBoxNodeRenderer;


public class SelectionTree extends JTree {

    private static final long serialVersionUID = -2328164272744518752L;

    private ArrayList<MouseListener> _mouseListeners = null;
    private ArrayList<TreeSelectionListener> _treeSelectionListeners = null;

    public SelectionTree(DefaultMutableTreeNode dmtn) {
        super(dmtn);
        init(false);
    }

    public SelectionTree(DefaultMutableTreeNode dmtn, boolean useEditor) {
        super(dmtn);
        init(useEditor);
    }

    private void init(boolean useEditor) {
        this.setCellRenderer(new CheckBoxNodeRenderer<>());
        if (useEditor) {
            this.setCellEditor(new CheckBoxNodeEditor(this));
            this.setEditable(true);
        }
    }

    public CheckBoxNodeRenderer<?> getCheckBoxNodeRenderer() {
        return ((CheckBoxNodeRenderer<?>) getCellRenderer());
    }

    public synchronized void addExtraMouseListener(MouseListener listener) {
        super.addMouseListener(listener);
        getExtraMouseListeners().add(listener);
    }

    public synchronized void addExtraTreeSelectionListener(TreeSelectionListener listener) {
        super.addTreeSelectionListener(listener);
        getExtraTreeSelectionListeners().add(listener);
    }

    public ArrayList<MouseListener> getExtraMouseListeners() {
        if (_mouseListeners == null) {
            _mouseListeners = new ArrayList<>();
        }
        return _mouseListeners;
    }

    public ArrayList<TreeSelectionListener> getExtraTreeSelectionListeners() {
        if (_treeSelectionListeners == null) {
            _treeSelectionListeners = new ArrayList<>();
        }
        return _treeSelectionListeners;
    }

    public void expand(SelectableNode<?> node) {
        expandPath(new TreePath(node.getPath()));
        Enumeration<?> list = node.children();
        while (list.hasMoreElements()) {
            SelectableNode<?> childNode = (SelectableNode<?>) list.nextElement();
            if (!node.isLeaf()) {
                expand(childNode);
            }
        }
    }

    public void collapse(SelectableNode<?> node) {
        int row = this.getRowCount() - 1;
        while (row >= 1) {
            this.collapseRow(row);
            row--;
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