package se.cambio.cds.gdl.editor.controller.sw;

import java.util.Collection;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.LanguageManager;
import se.cambio.cds.gdl.editor.view.dialog.DialogSplash;
import se.cambio.cds.openehr.view.applicationobjects.Archetypes;
import se.cambio.cds.openehr.view.applicationobjects.Templates;
import se.cambio.cds.openehr.view.dialogs.DialogLongMessageNotice;
import se.cambio.cds.openehr.view.dialogs.DialogLongMessageNotice.MessageType;
import se.cambio.cds.util.CDSSwingWorker;
import se.cambio.cds.util.CDSTerminologyService;
import se.cambio.cds.util.InitialLoadingObservable;
import se.cambio.cds.util.exceptions.InternalErrorException;

public class LoadEditorSW extends CDSSwingWorker{

    private DialogSplash _dialog = null;

    public LoadEditorSW(DialogSplash dialog){
	_dialog = dialog;
	InitialLoadingObservable.getDelegate().addObserver(_dialog);
    }

    @Override
    protected void executeCDSSW() throws InternalErrorException {
	CDSTerminologyService.getDelegate();
	Archetypes.loadArchetypes();
	Templates.loadTemplates();
    }

    protected void done() {
	try {
	    GDLEditor controller = new GDLEditor();
	    EditorManager.initController(controller);
	    _dialog.stop();
	    Collection<InternalErrorException> internalErrorExceptions =
		    InitialLoadingObservable.getLoadingExceptions();
	    if (!internalErrorExceptions.isEmpty()){
		StringBuffer errorsSB = new StringBuffer();
		for (InternalErrorException internalErrorException : internalErrorExceptions) {
		    errorsSB.append(internalErrorException.toString()+"\n");
		}
		DialogLongMessageNotice dialog = 
			new DialogLongMessageNotice(
				EditorManager.getActiveEditorWindow(), 
				LanguageManager.getMessage("ErrorsFoundLoading"),
				LanguageManager.getMessage("ErrorsFoundLoadingD"),
				errorsSB.toString(), MessageType.ERROR);
		dialog.setVisible(true);
	    }
	    EditorManager.getActiveEditorWindow().setVisible(true);
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  1.1 (the 'License'); you may not use this file except in compliance with
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