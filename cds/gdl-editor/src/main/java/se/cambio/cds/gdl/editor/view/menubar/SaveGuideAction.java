package se.cambio.cds.gdl.editor.view.menubar;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;


public class SaveGuideAction extends AbstractAction {

    private static final long serialVersionUID = -3561842193285119707L;
    private EditorManager editorManager;
    private GDLEditor gdlEditor;

    SaveGuideAction(EditorManager editorManager) {
        super();
        this.editorManager = editorManager;
        init();
    }

    public SaveGuideAction(GDLEditor gdlEditor) {
        super();
        this.gdlEditor = gdlEditor;
        init();
    }

    private void init() {
        putValue(NAME, GDLEditorLanguageManager.getMessage("SaveGuide"));
        putValue(SMALL_ICON, GDLEditorImageUtil.SAVE_ICON);
        putValue(SHORT_DESCRIPTION, GDLEditorLanguageManager.getMessage("SaveGuideSD"));
        putValue(LONG_DESCRIPTION, GDLEditorLanguageManager.getMessage("SaveGuideD"));
        putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_S, ActionEvent.CTRL_MASK));
    }

    public void actionPerformed(ActionEvent ev) {
        if (gdlEditor == null) {
            gdlEditor = editorManager.getActiveGDLEditor();
        }
        gdlEditor.save();
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