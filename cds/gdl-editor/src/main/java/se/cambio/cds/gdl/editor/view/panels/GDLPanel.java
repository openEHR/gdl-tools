package se.cambio.cds.gdl.editor.view.panels;

import jsyntaxpane.DefaultSyntaxKit;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLSyntaxKit;
import se.cambio.cds.view.swing.panel.interfaces.RefreshablePanel;

import javax.swing.*;
import java.awt.*;

public class GDLPanel extends JPanel implements RefreshablePanel{

    private static final long serialVersionUID = 1L;
    private JScrollPane mainScrollPanel;
    private JEditorPane editorPane;
    private GDLEditor controller = null;
    private JPanel mainPanel;
    private JLabel statusLabel;

    GDLPanel(GDLEditor controller){
        this.controller = controller;
        init();
    }

    public void init(){
        this.setLayout(new BorderLayout());
        DefaultSyntaxKit.initKit();
        DefaultSyntaxKit.registerContentType("text/gdl", GDLSyntaxKit.class.getCanonicalName());
        this.setFocusable(true);
        refresh();
    }

    private JPanel getMainPanel() {
        if (mainPanel == null) {
            mainPanel = new JPanel(new BorderLayout());
            mainPanel.add(getMainScrollPanel(), BorderLayout.CENTER);
            mainPanel.add(getStatusLabel(), BorderLayout.SOUTH);
        }
        return mainPanel;
    }

    private JScrollPane getMainScrollPanel(){
        if (mainScrollPanel==null){
            mainScrollPanel = new JScrollPane(getEditorPane());
        }
        return mainScrollPanel;
    }

    private JEditorPane getEditorPane(){
        if (editorPane ==null){
            editorPane = new JEditorPane();
            editorPane.setEditable(true);
        }
        return editorPane;
    }

    public String getGuideStr(){
        return getEditorPane().getText();
    }

    public void refresh(){
        if (mainPanel != null){
            remove(mainPanel);
            mainPanel = null;
            mainScrollPanel = null;
            editorPane = null;
        }
        this.add(getMainPanel());
        getEditorPane().setContentType("text/gdl");
        String gdlStr = controller.getSerializedEntity();
        if (gdlStr != null){
            getEditorPane().setText(gdlStr);
        }
        this.repaint();
        this.revalidate();
        SwingUtilities.invokeLater(() -> getMainScrollPanel().getVerticalScrollBar().setValue(0));
    }

    private JLabel getStatusLabel() {
        if (statusLabel == null) {
            statusLabel = new JLabel();
        }
        return statusLabel;
    }

    public void setStatus(StatusType statusType, String message) {
        getStatusLabel().setForeground(getStatusColor(statusType));
        getStatusLabel().setIcon(getStatusIcon(statusType));
        getStatusLabel().setText(message);
    }

    private Color getStatusColor(StatusType statusType) {
        if (statusType.equals(StatusType.ERRONEOUS)) {
            return Color.RED;
        } else {
            return Color.BLACK;
        }
    }

    private ImageIcon getStatusIcon(StatusType statusType) {
        if (statusType.equals(StatusType.ERRONEOUS)) {
            return GDLEditorImageUtil.EXCLAMATION_ICON;
        } else if (statusType.equals(StatusType.CORRECT)) {
            return GDLEditorImageUtil.ACCEPT_ICON;
        } else {
            return null;
        }
    }

    public enum StatusType {
        ERRONEOUS, CORRECT
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