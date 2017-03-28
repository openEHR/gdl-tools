package se.cambio.cds.gdl.editor.controller.sw;

import org.apache.commons.io.IOUtils;
import se.cambio.cds.gdl.editor.controller.*;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.menubar.MainMenuBar;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.util.CDSSwingWorker;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.view.util.WindowManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;


public class LoadGuideFromFileRSW extends CDSSwingWorker {

    private WindowManager windowManager;
    private EditorManager editorManager;
    private EditorFileManager editorFileManager;
    private GuidelineEditorManager guidelineEditorManager;
    private GdlEditorFactory gdlEditorFactory;
    private File guideFile = null;
    private MainMenuBar mainMenuBar;
    private GDLEditor editor = null;
    private String guideStr = null;

    public LoadGuideFromFileRSW(
            WindowManager windowManager,
            EditorManager editorManager,
            EditorFileManager editorFileManager,
            GuidelineEditorManager guidelineEditorManager,
            GdlEditorFactory gdlEditorFactory,
            MainMenuBar mainMenuBar,
            File guideFile) {
        super();
        this.windowManager = windowManager;
        this.editorManager = editorManager;
        this.editorFileManager = editorFileManager;
        this.guidelineEditorManager = guidelineEditorManager;
        this.gdlEditorFactory = gdlEditorFactory;
        this.guideFile = guideFile;
        this.mainMenuBar = mainMenuBar;
    }

    protected void executeCDSSW() throws InternalErrorException {
        try {
            if (guideFile != null) {
                FileInputStream fis = new FileInputStream(guideFile);
                guideStr = IOUtils.toString(fis, "UTF8");
                Guide guide = guidelineEditorManager.parseGuide(new ByteArrayInputStream(guideStr.getBytes("UTF8")));
                if (guide != null) {
                    editor = gdlEditorFactory.createGdlEditor(
                            guide,
                            editorManager.getActiveEditorViewer());
                }
            } else {
                this.cancel(true);
            }
        } catch (Exception e) {
            ExceptionHandler.handle(e);
        }
    }


    protected void done() {
        try {
            if (editor != null) {
                if (guidelineEditorManager.checkParsedGuide(guideStr, editor.getEntity())) {
                    editorFileManager.setLastFileLoaded(guideFile);
                    editorFileManager.setLastFolderLoaded(guideFile.getParentFile());
                    editorManager.initController(editor);
                    mainMenuBar.refreshLanguageMenu();
                }
            }
        } finally {
            windowManager.setFree();
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