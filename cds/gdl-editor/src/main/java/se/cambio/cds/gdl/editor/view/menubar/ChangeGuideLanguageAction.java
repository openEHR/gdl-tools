/*
 * Created on 30-ago-2006
 *


 */
package se.cambio.cds.gdl.editor.view.menubar;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import sun.applet.Main;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class ChangeGuideLanguageAction extends AbstractAction {

    private static final long serialVersionUID = -3561842193285119707L;
    private String language = null;
    private EditorManager editorManager;
    private MainMenuBar mainMenuBar;

    ChangeGuideLanguageAction(String language, EditorManager editorManager, MainMenuBar mainMenuBar) {
        super();
        this.language = language;
        this.editorManager = editorManager;
        this.mainMenuBar = mainMenuBar;
        putValue(NAME, this.language);
        putValue(SMALL_ICON, null);
        putValue(SHORT_DESCRIPTION, this.language/*TODO Add desc*/);
        putValue(LONG_DESCRIPTION, this.language/*TODO Add desc*/);
    }

    public void actionPerformed(ActionEvent e) {
        editorManager.getActiveEditorController().changeLanguage(language);
        mainMenuBar.refreshLanguageMenu();

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