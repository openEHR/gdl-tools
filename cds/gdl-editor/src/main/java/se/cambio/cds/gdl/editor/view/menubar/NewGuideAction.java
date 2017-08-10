/*
 * Created on 30-ago-2006
 *


 */
package se.cambio.cds.gdl.editor.view.menubar;

import se.cambio.cds.gdl.editor.controller.EditorFileManager;
import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.controller.GdlEditorFactory;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.model.Guide;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;


public class NewGuideAction extends AbstractAction {

    private static final long serialVersionUID = -3561842193285119707L;
    private EditorManager editorManager;
    private GdlEditorFactory gdlEditorFactory;
    private MainMenuBar mainMenuBar;
    private EditorFileManager editorFileManager;

    NewGuideAction(EditorManager editorManager, EditorFileManager editorFileManager, GdlEditorFactory gdlEditorFactory, MainMenuBar mainMenuBar) {
        super();
        this.editorFileManager = editorFileManager;
        this.editorManager = editorManager;
        this.gdlEditorFactory = gdlEditorFactory;
        this.mainMenuBar = mainMenuBar;
        putValue(NAME, GDLEditorLanguageManager.getMessage("CreateNewGuide"));
        putValue(SMALL_ICON, null);
        putValue(SHORT_DESCRIPTION, GDLEditorLanguageManager.getMessage("CreateNewGuideSD"));
        putValue(LONG_DESCRIPTION, GDLEditorLanguageManager.getMessage("CreateNewGuideD"));
        putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_N, ActionEvent.CTRL_MASK));
    }

    public void actionPerformed(ActionEvent e) {
        editorManager.runIfOkWithCurrentEditor(
                () -> {
                    GDLEditor controller = gdlEditorFactory.createGdlEditor(
                            new Guide(),
                            editorManager.getActiveEditorViewer());
                    editorFileManager.setLastFileLoaded(null);
                    editorManager.initController(controller);
                    mainMenuBar.refreshLanguageMenu();
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