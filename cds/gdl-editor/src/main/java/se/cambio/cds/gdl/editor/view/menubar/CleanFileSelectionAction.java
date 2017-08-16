package se.cambio.cds.gdl.editor.view.menubar;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.JFileChooser;
import javax.swing.JTextField;

import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;

public class CleanFileSelectionAction extends AbstractAction {

    private static final long serialVersionUID = -2323804790429232264L;
    private JFileChooser fileChooser = null;
    private JTextField textField = null;

    public CleanFileSelectionAction(JFileChooser fileChooser, JTextField textField) {
        this.fileChooser = fileChooser;
        this.textField = textField;
        putValue(NAME, GDLEditorLanguageManager.getMessage("CleanSelectFolder"));
        putValue(SMALL_ICON, GDLEditorImageUtil.CLEAR_ICON);
        putValue(SHORT_DESCRIPTION, GDLEditorLanguageManager.getMessage("CleanSelectFolderSD"));
        putValue(LONG_DESCRIPTION, GDLEditorLanguageManager.getMessage("CleanSelectFolderD"));
        this.setEnabled(true);
    }

    public void actionPerformed(ActionEvent ev) {
        fileChooser.setSelectedFile(null);
        textField.setText("");
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