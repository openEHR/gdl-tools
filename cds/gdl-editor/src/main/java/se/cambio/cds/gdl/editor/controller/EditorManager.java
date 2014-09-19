package se.cambio.cds.gdl.editor.controller;

import se.cambio.cds.gdl.editor.controller.interfaces.EditorController;
import se.cambio.cds.gdl.editor.controller.interfaces.EditorViewer;
import se.cambio.cds.gdl.editor.view.dialog.DialogEditor;
import se.cambio.cds.gdl.editor.view.frame.EditorFrame;
import se.cambio.cds.gdl.editor.view.menubar.MainMenuBar;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.awt.*;
import java.io.File;

public class EditorManager {
    private static EditorManager _instance = null;
    private EditorController _controller = null;
    private Window _editorViewer = null;
    private boolean _exitOnClose = true;

    private File _lastFolderLoaded = null;
    private File _lastFileLoaded = null;
    private boolean _checkOnExit = true;

    private EditorManager(){
    }

    public static GDLEditor getActiveGDLEditor(){
        if (getDelegate()._controller instanceof GDLEditor){
            return (GDLEditor)getActiveEditorController();
        }else{
            return null;
        }
    }

    public static EditorController getActiveEditorController(){
        return getDelegate()._controller;
    }

    public static Window getActiveEditorWindow(){
        return getDelegate()._editorViewer;
    }

    public static EditorViewer getActiveEditorViewer(){
        return ((EditorViewer)getDelegate()._editorViewer);
    }

    public static MainMenuBar getMainMenuBar(){
        return getActiveEditorViewer().getMainMenuBar();
    }

    public static void setExitOnClose(boolean exitOnClose){
        getDelegate()._exitOnClose = exitOnClose;
    }

    public static boolean getExitOnClose(){
        return getDelegate()._exitOnClose;
    }

    public static void closeEditor(){
        Runnable exitRunnable = new Runnable() {
            @Override
            public void run() {
                getDelegate()._editorViewer.dispose();
                if (getDelegate()._exitOnClose){
                    System.exit(0);
                }
            }
        };
        if (getDelegate()._controller!=null && checkOnExit()){
            getDelegate()._controller.runIfOKToExit(exitRunnable);
        }else{
            exitRunnable.run();
        }
    }

    public static void initController(EditorController controller) throws InternalErrorException{
        getDelegate()._controller = controller;
        if (getDelegate()._editorViewer!=null){
            getActiveEditorViewer().initController(controller);
        }else{
            throw new InternalErrorException(new Exception("createEditorFrame or createEditorDialog must be called before this method."));
        }
    }

    public static EditorFrame createEditorFrame(){
        EditorFrame ef = new EditorFrame();
        getDelegate()._editorViewer = ef;
        setExitOnClose(true);
        return ef;
    }


    public static DialogEditor createEditorDialog(Window owner){
        DialogEditor ed = new DialogEditor(owner);
        getDelegate()._editorViewer = ed;
        setExitOnClose(false);
        return ed;
    }

    public static File getLastFolderLoaded() {
        if (getDelegate()._lastFolderLoaded==null){
            getDelegate()._lastFolderLoaded = UserConfigurationManager.getGuidesFolder();
        }
        return getDelegate()._lastFolderLoaded;
    }

    public static void setLastFolderLoaded(File _lastFolderLoaded) {
        getDelegate()._lastFolderLoaded = _lastFolderLoaded;
    }

    public static File getLastFileLoaded() {
        return getDelegate()._lastFileLoaded;
    }

    public static void setLastFileLoaded(File _lastFileLoaded) {
        getDelegate()._lastFileLoaded = _lastFileLoaded;
    }

    public static boolean checkOnExit(){
        return getDelegate()._checkOnExit;
    }

    public static void setCheckOnExit(boolean checkOnExit){
        getDelegate()._checkOnExit = checkOnExit;
    }


    public static void runIfOkWithCurrentEditor(Runnable runnable){
        EditorController editorController = getActiveEditorController();
        if(editorController!=null){
            editorController.runIfOKToExit(runnable);
        }else{
            runnable.run();
        }
    }

    public static void requestFocusInWindow(){
        EditorController editorController = getActiveEditorController();
        if(editorController!=null){
            editorController.getEditorPanel().requestFocusInWindow();
        }
    }

    public static EditorManager getDelegate(){
        if (_instance==null){
            _instance = new EditorManager();
        }
        return _instance;
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