package se.cambio.cds.gdl.editor.controller;

import se.cambio.cds.gdl.editor.controller.interfaces.EditorController;
import se.cambio.cds.gdl.editor.controller.interfaces.EditorViewer;
import se.cambio.openehr.view.util.WindowManager;

import java.awt.*;

public class EditorManager {
    private WindowManager windowManager;
    private EditorController controller = null;
    private boolean checkOnExit = true;
    private Window editorViewer = null;
    private boolean exitOnClose = true;

    public EditorManager(
            WindowManager windowManager) {
        this.windowManager = windowManager;
    }

    public GDLEditor getActiveGDLEditor() {
        if (controller instanceof GDLEditor) {
            return (GDLEditor) getActiveEditorController();
        } else {
            return null;
        }
    }

    public EditorController getActiveEditorController() {
        return controller;
    }

    public Window getActiveEditorWindow() {
        return editorViewer;
    }

    public EditorViewer getActiveEditorViewer() {
        return ((EditorViewer) editorViewer);
    }

    public void initController(EditorController controller) {
        boolean close = true;
        if (this.controller != null) {
            close = this.controller.close();
        }
        if (close) {
            this.controller = controller;
            if (editorViewer != null) {
                getActiveEditorViewer().initController(controller);
            } else {
                throw new RuntimeException("createEditorFrame or createEditorDialog must be called before this method.");
            }
        } else {
            controller.close();
        }
    }


    public boolean checkOnExit() {
        return checkOnExit;
    }

    public void setCheckOnExit(boolean checkOnExit) {
        this.checkOnExit = checkOnExit;
    }


    public void runIfOkWithCurrentEditor(Runnable runnable) {
        EditorController editorController = getActiveEditorController();
        if (editorController != null) {
            editorController.runIfOKToExit(runnable);
        } else {
            runnable.run();
        }
    }

    public void requestFocusInWindow() {
        EditorController editorController = getActiveEditorController();
        if (editorController != null) {
            editorController.getEditorPanel().requestFocusInWindow();
        }
    }

    public void closeEditor() {
        Runnable exitRunnable = () -> {
            editorViewer.dispose();
            if (exitOnClose) {
                System.exit(0);
            }
        };
        if (controller != null && checkOnExit()) {
            controller.runIfOKToExit(exitRunnable);
        } else {
            exitRunnable.run();
        }
    }

    public void setExitOnClose(boolean exitOnClose) {
        this.exitOnClose = exitOnClose;
    }

    public boolean getExitOnClose() {
        return exitOnClose;
    }

    public void setEditorViewer(Window editorViewer) {
        this.editorViewer = editorViewer;
    }


    public WindowManager getWindowManager() {
        return windowManager;
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