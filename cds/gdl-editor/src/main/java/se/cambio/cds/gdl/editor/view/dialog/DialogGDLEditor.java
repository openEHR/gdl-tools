/*
 * Created on 26-oct-2006
 *


 */
package se.cambio.cds.gdl.editor.view.dialog;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.controller.interfaces.EditorViewer;
import se.cambio.cds.gdl.editor.view.menubar.MainMenuBar;
import se.cambio.openehr.view.util.ScreenUtil;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;


/**
 * @author icorram
 *
 */

public class DialogGDLEditor extends JDialog implements EditorViewer{


    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private MainMenuBar principalMenuBar;

    /**
     * This is the default constructor
     */
    public DialogGDLEditor(Window owner) {
        super(owner, "", ModalityType.APPLICATION_MODAL);
        initialize();
    }

    /**
     * This method initializes this
     */
    private  void initialize() {
        this.setSize(new Dimension(1024, 768));
        ScreenUtil.centerComponentOnScreen(this, this.getOwner());
        this.setJMenuBar(getMainMenuBar());
        this.setResizable(true);
        this.addWindowListener(new CancelarCambiosAction());
        this.setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
    }

    public void initController(GDLEditor controller) {
        setTitle(controller.getTitle());
        setContent(controller.getEditorPanel());
    }

    protected class CancelarCambiosAction extends WindowAdapter{

        public void windowOpened(WindowEvent e){
        }

        public void actionPerformed(ActionEvent e) {
            EditorManager.closeEditor();
        }

        public void windowClosing(WindowEvent we) {
            EditorManager.closeEditor();
        }
    }

    public MainMenuBar getMainMenuBar() {
        if (principalMenuBar == null) {
            principalMenuBar = new MainMenuBar();
        }
        return principalMenuBar;
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