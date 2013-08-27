package se.cambio.cds.gdl.editor.controller.sw;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.util.CDSSwingWorker;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.io.*;

/**
 * @author iago.corbal
 */
public class SaveGuideOnFileRSW extends CDSSwingWorker {

    private File _guideFile = null;
    private GDLEditor _controller = null;
    private String _guideStr = null;

    public SaveGuideOnFileRSW(File guideFile) {
        super();
        _guideFile = guideFile;
        _controller = EditorManager.getActiveGDLEditor();
        if (_guideFile==null){
            JFileChooser fileChooser = new JFileChooser(EditorManager.getLastFolderLoaded());
            FileNameExtensionFilter filter =
                    new FileNameExtensionFilter(
                            GDLEditorLanguageManager.getMessage("Guide"),new String[]{"gdl"});
            fileChooser.setDialogTitle(GDLEditorLanguageManager.getMessage("SaveGuide"));
            fileChooser.setFileFilter(filter);
            String guideId = _controller.getIdGuide();
            if (guideId==null){
                GDLEditorLanguageManager.getMessage("Guide");
            }
            File file = new File(guideId+".gdl");
            fileChooser.setSelectedFile(file);
            int result = fileChooser.showSaveDialog(EditorManager.getActiveEditorWindow());
            _guideFile = fileChooser.getSelectedFile();
            if (result == JFileChooser.CANCEL_OPTION){
                _guideFile = null;
            }else{
                //All files must end with .gdl
                String absolutePath = _guideFile.getAbsolutePath();
                if (!absolutePath.toLowerCase().endsWith(".gdl")){
                    _guideFile = new File(absolutePath+".gdl");
                }
                //Set guide Id
                guideId = getGuideIdFromFile(_guideFile);
                _controller.setIdGuide(guideId);
            }
        }
        if (_guideFile!=null){
            //Check guide Id
            String guideId = getGuideIdFromFile(_guideFile);
            if (!guideId.equals(_controller.getIdGuide())){
                int result =
                        JOptionPane.showConfirmDialog(
                                EditorManager.getActiveEditorWindow(),
                                GDLEditorLanguageManager.getMessage("ChangeOfGuideIdFound", new String[]{_controller.getIdGuide(), guideId}));
                if (result==JOptionPane.CANCEL_OPTION){
                    _guideFile = null;
                }else{
                    if (result==JOptionPane.YES_OPTION){
                        _controller.setIdGuide(guideId);
                    }
                }
            }
            _guideStr = _controller.serializeCurrentGuide();
        }
    }

    protected void executeCDSSW()  throws InternalErrorException{
        if (_guideFile != null && _guideStr!=null && !_guideStr.isEmpty()){
            try {
                FileOutputStream fos = new FileOutputStream(_guideFile);
                OutputStreamWriter output = new OutputStreamWriter(fos, "UTF-8");
                //FileWriter always assumes default encoding is OK!
                output.write(_guideStr);
                output.close();
            } catch (FileNotFoundException e) {
                new InternalErrorException(e);
            } catch (IOException e) {
                new InternalErrorException(e);
            }
        }else{
            this.cancel(true);
        }
    }

    public File getFile(){
        return _guideFile;
    }

    private static String getGuideIdFromFile(File guideFile){
        String guideId = guideFile.getName();
        if (guideId.toLowerCase().endsWith(".gdl")){
            guideId = guideId.substring(0, guideId.length()-4);
        }
        return guideId;
    }

    protected void done() {
        if (_guideFile!=null && _guideStr!=null && !_guideStr.isEmpty()){
            EditorManager.getActiveGDLEditor().updateOriginal();
            EditorManager.setLastFileLoaded(_guideFile);
            EditorManager.setLastFolderLoaded(_guideFile.getParentFile());
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