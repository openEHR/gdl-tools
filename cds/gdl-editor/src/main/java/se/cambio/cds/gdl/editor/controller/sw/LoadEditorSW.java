package se.cambio.cds.gdl.editor.controller.sw;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.controller.GdlEditorFactory;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.menubar.MainMenuBar;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.util.CDSSwingWorker;
import se.cambio.openehr.controller.InitialLoadingObservable;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.view.dialogs.DialogLongMessageNotice;
import se.cambio.openehr.view.dialogs.DialogLongMessageNotice.MessageType;

import java.util.Collection;

public class LoadEditorSW extends CDSSwingWorker {

    private GdlEditorFactory gdlEditorFactory;
    private EditorManager editorManager;
    private MainMenuBar mainMenuBar;

    public LoadEditorSW(
            GdlEditorFactory gdlEditorFactory,
            EditorManager editorManager,
            MainMenuBar mainMenuBar) {
        this.gdlEditorFactory = gdlEditorFactory;
        this.editorManager = editorManager;
        this.mainMenuBar = mainMenuBar;
    }

    @Override
    protected void executeCDSSW() throws InternalErrorException {
    }

    protected void done() {
        GDLEditor controller = gdlEditorFactory.createGdlEditor(
                new Guide(),
                editorManager.getActiveEditorViewer());
        editorManager.initController(controller);
        Collection<InternalErrorException> internalErrorExceptions =
                InitialLoadingObservable.getLoadingExceptions();
        if (!internalErrorExceptions.isEmpty()) {
            StringBuilder errorsSB = new StringBuilder();
            for (InternalErrorException internalErrorException : internalErrorExceptions) {
                errorsSB.append(internalErrorException.toString()).append("\n");
            }
            DialogLongMessageNotice dialog =
                    new DialogLongMessageNotice(
                            editorManager.getActiveEditorWindow(),
                            GDLEditorLanguageManager.getMessage("ErrorsFoundLoading"),
                            GDLEditorLanguageManager.getMessage("ErrorsFoundLoadingD"),
                            errorsSB.toString(), MessageType.ERROR);
            dialog.setVisible(true);
        }
        mainMenuBar.refreshLanguageMenu();
        editorManager.getActiveEditorWindow().setVisible(true);
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