package se.cambio.cds.gdl.editor.view.panels;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.util.export.html.GuideHTMLExporter;
import se.cambio.cds.view.swing.panel.interfaces.RefreshablePanel;

import javax.swing.*;
import java.awt.*;

public class HTMLPanel extends JPanel implements RefreshablePanel {

    private static final long serialVersionUID = 1L;
    private JScrollPane mainScrollPanel;
    private JEditorPane editorPanel;
    private GDLEditor controller = null;
    private GuideHTMLExporter guideHTMLExporter;

    HTMLPanel(GDLEditor controller, GuideHTMLExporter guideHTMLExporter) {
        this.controller = controller;
        this.guideHTMLExporter = guideHTMLExporter;
        init();
    }

    public void init() {
        this.setLayout(new BorderLayout());
        refresh();
    }

    private JScrollPane getMainScrollPanel() {
        if (mainScrollPanel == null) {
            mainScrollPanel = new JScrollPane();
            mainScrollPanel.setViewportView(getEditorPanel());
        }
        return mainScrollPanel;
    }

    private JEditorPane getEditorPanel() {
        if (editorPanel == null) {
            editorPanel = new JEditorPane();
            editorPanel.setContentType("text/html");
            editorPanel.setEditable(false);
            String html = guideHTMLExporter.convertToHTML(controller.getEntity(), controller.getCurrentLanguageCode());
            if (html != null) {
                editorPanel.setText(html);
            }
        }
        return editorPanel;
    }

    public void refresh() {
        if (mainScrollPanel != null) {
            remove(mainScrollPanel);
            mainScrollPanel = null;
            editorPanel = null;
        }
        this.add(getMainScrollPanel());
        this.revalidate();
        this.repaint();
        SwingUtilities.invokeLater(() -> getMainScrollPanel().getVerticalScrollBar().setValue(0));
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