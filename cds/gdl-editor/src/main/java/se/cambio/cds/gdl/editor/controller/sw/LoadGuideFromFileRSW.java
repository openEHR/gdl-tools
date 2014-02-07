package se.cambio.cds.gdl.editor.controller.sw;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.util.CDSSwingWorker;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.WindowManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;

/**
 * @author iago.corbal
 *
 */
public class LoadGuideFromFileRSW extends CDSSwingWorker {

    private File _guideFile = null;
    private GDLEditor _editor = null;
    private String _guideStr = null;

    public LoadGuideFromFileRSW() {
        super();
        init();
    }

    public LoadGuideFromFileRSW(File guideFile) {
        super();
        _guideFile = guideFile;
        init();
    }

    private void init(){
        if (_guideFile==null){
            JFileChooser fileChooser = new JFileChooser(EditorManager.getLastFolderLoaded());
            FileNameExtensionFilter filter = new FileNameExtensionFilter(
                    GDLEditorLanguageManager.getMessage("Guide"),new String[]{"gdl"});
            fileChooser.setDialogTitle(GDLEditorLanguageManager.getMessage("LoadGuide"));
            fileChooser.setFileFilter(filter);
            int result = fileChooser.showOpenDialog(EditorManager.getActiveEditorWindow());
            if (result != JFileChooser.CANCEL_OPTION){
                _guideFile = fileChooser.getSelectedFile();
            }
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    WindowManager.setBusy(GDLEditorLanguageManager.getMessage("Loading") + "...");
                }
            });
        }
    }

    protected void executeCDSSW() throws InternalErrorException {
        try{
            if (_guideFile!=null){
                FileInputStream fis = new FileInputStream(_guideFile);
                InputStreamReader in = new InputStreamReader(fis, "UTF-8");
                _guideStr = IOUtils.toString(in);
                Guide guide = GDLEditor.parseGuide(new ByteArrayInputStream(_guideStr.getBytes()));
                if (guide!=null){
                    _editor = new GDLEditor(guide);
                }
            }else{
                this.cancel(true);
            }
        }catch(Exception e){
            ExceptionHandler.handle(e);
        }
    }


    protected void done() {
        try{
            if (_editor!=null){
                if (GDLEditor.checkParsedGuide(_guideStr, _editor.getGuide())){
                    EditorManager.setLastFileLoaded(_guideFile);
                    EditorManager.setLastFolderLoaded(_guideFile.getParentFile());
                    try {
                        EditorManager.initController(_editor);
                    } catch (InternalErrorException e) {
                        ExceptionHandler.handle(e);
                    }
                }
            }
        }catch (Throwable th){
            ExceptionHandler.handle(th);
        }finally{
            WindowManager.setFree();
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