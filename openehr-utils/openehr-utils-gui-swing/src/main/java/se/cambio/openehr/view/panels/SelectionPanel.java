package se.cambio.openehr.view.panels;

import se.cambio.openehr.controller.sw.ExpandTreeRSW;
import se.cambio.openehr.controller.sw.FilterTreeRSW;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.view.trees.SelectableNode;
import se.cambio.openehr.view.trees.SelectionTree;
import se.cambio.openehr.view.util.WindowManager;

import javax.swing.*;
import javax.swing.event.TreeSelectionListener;
import javax.swing.plaf.TreeUI;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;


public class SelectionPanel extends JPanel {

    private static final long serialVersionUID = -7877248591456367314L;
    private SelectionTree jTree = null;
    private JScrollPane jScrollPane = null;
    private JPanel jPanelFiltro;
    private SelectableNode<?> rootNode = null;
    private TextWithCleanButtonPanel textWithCleanButtonPanel = null;
    private JButton expandButton = null;
    private JButton contractButton = null;
    private boolean useEditor = true;
    private boolean bigList = false;
    private JButton searchButton;
    private WindowManager windowManager;

    public SelectionPanel(WindowManager windowManager, SelectableNode<?> rootNode) {
        super();
        this.windowManager = windowManager;
        this.rootNode = rootNode;
        initialize();
    }

    public SelectionPanel(WindowManager windowManager, SelectableNode<?> rootNode, boolean useEditor) {
        super();
        this.windowManager = windowManager;
        this.rootNode = rootNode;
        this.useEditor = useEditor;
        initialize();
    }

    public SelectionPanel(WindowManager windowManager, SelectableNode<?> rootNode, boolean useEditor, boolean bigList) {
        super();
        this.windowManager = windowManager;
        this.rootNode = rootNode;
        this.useEditor = useEditor;
        this.bigList = bigList;
        initialize();
    }

    private void initialize() {
        this.setLayout(new BorderLayout());
        this.add(getFilterPanel(), BorderLayout.NORTH);
        this.add(getJScrollPane(), BorderLayout.CENTER);
    }

    public SelectableNode<?> getNode() {
        return rootNode;
    }

    public JPanel getFilterPanel() {
        if (jPanelFiltro == null) {
            jPanelFiltro = new JPanel(new FlowLayout(FlowLayout.LEFT));
            jPanelFiltro.add(new JLabel(OpenEHRLanguageManager.getMessage("Filter") + " :"));
            jPanelFiltro.add(getTextWithCleanButtonPanel());
            if (bigList) {
                jPanelFiltro.add(getSearchButton());
            }
            jPanelFiltro.add(getExpandButton());
            jPanelFiltro.add(getContractButton());
        }
        return jPanelFiltro;
    }

    public TextWithCleanButtonPanel getTextWithCleanButtonPanel() {
        if (textWithCleanButtonPanel == null) {
            textWithCleanButtonPanel = new TextWithCleanButtonPanel();
            textWithCleanButtonPanel.setPreferredSize(new Dimension(100, 20));
            if (bigList) {
                textWithCleanButtonPanel.getJButton().addActionListener(e -> filter());
                textWithCleanButtonPanel.getJTextField().addActionListener(e -> filter());
            } else {
                textWithCleanButtonPanel.addKeyListener(new FilerSelectionKeyListener());
            }
        }
        return textWithCleanButtonPanel;
    }

    private JButton getSearchButton() {
        if (searchButton == null) {
            searchButton = new JButton();
            searchButton.setIcon(OpenEHRImageUtil.SEARCH_ICON);
            searchButton.setToolTipText("Search");
            searchButton.setBackground(null);
            searchButton.setContentAreaFilled(false);
            searchButton.setBorderPainted(false);
            searchButton.setPreferredSize(new Dimension(16, 16));
            searchButton.addActionListener(e -> filter());
        }
        return searchButton;
    }


    private JButton getExpandButton() {
        if (expandButton == null) {
            expandButton = new JButton(new ExpandTreeAction());
            expandButton.setIcon(OpenEHRImageUtil.EXPAND_ICON);
            expandButton.setToolTipText(OpenEHRLanguageManager.getMessage("ExpandTreeD"));
            expandButton.setBackground(null);
            expandButton.setContentAreaFilled(false);
            expandButton.setBorderPainted(false);
            expandButton.setPreferredSize(new Dimension(16, 16));
        }
        return expandButton;
    }

    private SelectionPanel getSelectionPanel() {
        return this;
    }

    private class ExpandTreeAction extends AbstractAction {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent ev) {
            if (bigList) {
                new ExpandTreeRSW(windowManager, getSelectionPanel()).execute();
            } else {
                TreeUI ui = getJTree().getUI();
                getJTree().setUI(null);
                getJTree().expand(rootNode);
                getJTree().setUI(ui);
            }
        }
    }

    public JButton getContractButton() {
        if (contractButton == null) {
            contractButton = new JButton(new ContractTreeAction());
            contractButton.setIcon(OpenEHRImageUtil.CONTRACT_ICON);
            contractButton.setToolTipText(OpenEHRLanguageManager.getMessage("ContractTreeD"));
            contractButton.setBackground(null);
            contractButton.setContentAreaFilled(false);
            contractButton.setBorderPainted(false);
            contractButton.setPreferredSize(new Dimension(16, 16));
        }
        return contractButton;
    }

    private class ContractTreeAction extends AbstractAction {

        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent ev) {
            getJTree().collapse(rootNode);
        }
    }

    public SelectionTree getJTree() {
        if (jTree == null) {
            jTree = new SelectionTree(rootNode, useEditor);
        }
        return jTree;
    }

    public JScrollPane getJScrollPane() {
        if (jScrollPane == null) {
            jScrollPane = new JScrollPane();
            jScrollPane.setViewportView(getJTree());
        }
        return jScrollPane;
    }

    public void changeRootNode(SelectableNode<?> node) {
        rootNode = node;

        ArrayList<MouseListener> listeners = getJTree().getExtraMouseListeners();
        jTree = null;
        getJScrollPane().remove(getJTree());
        for (MouseListener listener : listeners) {
            getJTree().addExtraMouseListener(listener);
        }
        ArrayList<TreeSelectionListener> tsListeners = getJTree().getExtraTreeSelectionListeners();
        for (TreeSelectionListener listener : tsListeners) {
            getJTree().addExtraTreeSelectionListener(listener);
        }
        getJScrollPane().setViewportView(getJTree());
        getJScrollPane().revalidate();
        getJScrollPane().repaint();
    }

    public class FilerSelectionKeyListener implements KeyListener {

        public void keyPressed(KeyEvent ev) {
        }

        public void keyReleased(KeyEvent ev) {
            filter();
        }

        public void keyTyped(KeyEvent ev) {

        }
    }

    private void filter() {
        if (bigList) {
            new FilterTreeRSW(windowManager, this).execute();
        } else {
            FilterTreeRSW.filterNode(this);
            getJTree().expand(getNode());
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