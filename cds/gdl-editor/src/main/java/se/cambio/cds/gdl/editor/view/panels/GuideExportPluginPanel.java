package se.cambio.cds.gdl.editor.view.panels;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.controller.exportplugins.GuideExportPlugin;
import se.cambio.cds.gdl.editor.view.panels.interfaces.RefreshablePanel;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import java.awt.*;

public class GuideExportPluginPanel extends JPanel implements RefreshablePanel{

    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private GuideExportPlugin _exportPlugin = null;
    private JScrollPane mainScrollPanel;
    private JTextArea textArea;
    private GDLEditor _controller = null;
    public GuideExportPluginPanel(GDLEditor controller, GuideExportPlugin exportPlugin){
        _exportPlugin = exportPlugin;
        _controller = controller;
        init();
    }

    public void init(){
        this.setLayout(new BorderLayout());
        refresh();
    }

    private JScrollPane getMainScrollPanel(){
        if (mainScrollPanel==null){
            mainScrollPanel = new JScrollPane();
            mainScrollPanel.setViewportView(getTextArea());
        }
        return mainScrollPanel;
    }

    private JTextArea getTextArea(){
        if (textArea==null){
            textArea = new JTextArea();
            Guide guide = _controller.getGuide();
            if (guide!=null){
                try {
                    textArea.setText(_exportPlugin.getExportedGuide(guide));
                } catch (InternalErrorException e) {
                    ExceptionHandler.handle(e);
                }
            }
            textArea.setEditable(false);
        }
        return textArea;
    }

    public void refresh(){
        if (mainScrollPanel!=null){
            remove(mainScrollPanel);
            mainScrollPanel = null;
            textArea = null;
        }
        this.add(getMainScrollPanel());
        this.revalidate();
        this.repaint();
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                getMainScrollPanel().getVerticalScrollBar().setValue(0);
            }
        });
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