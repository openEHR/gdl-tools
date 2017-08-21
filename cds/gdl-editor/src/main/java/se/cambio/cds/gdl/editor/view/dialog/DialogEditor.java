package se.cambio.cds.gdl.editor.view.dialog;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.interfaces.EditorController;
import se.cambio.cds.gdl.editor.controller.interfaces.EditorViewer;
import se.cambio.cds.gdl.editor.view.menubar.MainMenuBar;
import se.cambio.openehr.view.util.ScreenUtil;

import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;


public class DialogEditor extends JDialog implements EditorViewer {

    private static final long serialVersionUID = 1L;
    private EditorManager editorManager;
    private MainMenuBar mainMenuBar;

    public DialogEditor(Window owner, EditorManager editorManager, MainMenuBar mainMenuBar) {
        super(owner, "", ModalityType.APPLICATION_MODAL);
        this.editorManager = editorManager;
        this.mainMenuBar = mainMenuBar;
        initialize();
    }

    private void initialize() {
        this.setSize(new Dimension(1024, 768));
        ScreenUtil.centerComponentOnScreen(this, this.getOwner());
        this.setJMenuBar(mainMenuBar);
        this.setResizable(true);
        this.addWindowListener(new CancelChanges());
        this.setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
    }

    public void initController(EditorController controller) {
        setTitle(controller.getTitle());
        setContent(controller.getEditorPanel());
    }

    protected class CancelChanges extends WindowAdapter {

        @Override
        public void windowOpened(WindowEvent event) {
        }

        @Override
        public void windowClosing(WindowEvent we) {
            editorManager.closeEditor();
        }
    }

    @Override
    public void setContent(JPanel panel) {
        setContentPane(panel);
        this.validate();
        this.repaint();
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