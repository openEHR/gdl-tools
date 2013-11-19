package se.cambio.cds.gdl.editor.controller.sw;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.dialog.DialogSplash;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.util.CDSSwingWorker;
import se.cambio.openehr.controller.InitialLoadingObservable;
import se.cambio.openehr.controller.InitialLoadingObservable.LoadingStage;
import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.controller.terminology.session.data.Terminologies;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.view.dialogs.DialogLongMessageNotice;
import se.cambio.openehr.view.dialogs.DialogLongMessageNotice.MessageType;

import java.util.Collection;

public class LoadEditorSW extends CDSSwingWorker{

    private DialogSplash _dialog = null;

    public LoadEditorSW(DialogSplash dialog){
        _dialog = dialog;
        InitialLoadingObservable.getDelegate().addObserver(_dialog);
    }

    @Override
    protected void executeCDSSW() throws InternalErrorException {
        InitialLoadingObservable.setCurrentLoadingStage(LoadingStage.ONTOLOGIES);
        //TODO Load ontologies
        InitialLoadingObservable.setCurrentLoadingStageFinished();
        InitialLoadingObservable.setCurrentLoadingStage(LoadingStage.TERMINOLOGIES);
        Terminologies.loadTerminologies();
        OpenEHRSessionManager.getTerminologyFacadeDelegate(); //TODO Init terminology
        InitialLoadingObservable.setCurrentLoadingStageFinished();
        InitialLoadingObservable.setCurrentLoadingStage(LoadingStage.ARCHETYPES);
        Archetypes.loadArchetypes();
        InitialLoadingObservable.setCurrentLoadingStageFinished();
        InitialLoadingObservable.setCurrentLoadingStage(LoadingStage.TEMPLATES);
        Templates.loadTemplates();
        InitialLoadingObservable.setCurrentLoadingStageFinished();
    }

    protected void done() {
        try {
            GDLEditor controller = new GDLEditor(new Guide());
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
                                GDLEditorLanguageManager.getMessage("ErrorsFoundLoading"),
                                GDLEditorLanguageManager.getMessage("ErrorsFoundLoadingD"),
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