package se.cambio.cds.gdl.editor.controller.sw;

import se.cambio.cds.gdl.editor.controller.EditorFileManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.util.CDSSwingWorker;
import se.cambio.openehr.view.util.WindowManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.io.*;


public class SaveGuideOnFileRSW extends CDSSwingWorker {

    private final WindowManager windowManager;
    private File guideFile = null;
    private String guideStr = null;
    private GDLEditor controller;

    public SaveGuideOnFileRSW(WindowManager windowManager, File guideFile, GDLEditor controller, EditorFileManager editorFileManager) {
        super();
        this.windowManager = windowManager;
        this.guideFile = guideFile;
        this.controller = controller;
        if (this.guideFile == null) {
            JFileChooser fileChooser = new JFileChooser(editorFileManager.getLastFolderLoaded());
            FileNameExtensionFilter filter =
                    new FileNameExtensionFilter(
                            GDLEditorLanguageManager.getMessage("Guide"), "gdl");
            fileChooser.setDialogTitle(GDLEditorLanguageManager.getMessage("SaveGuide"));
            fileChooser.setFileFilter(filter);
            String guideId = controller.getEntityId();
            if (guideId == null) {
                GDLEditorLanguageManager.getMessage("Guide");
            }
            File file = new File(guideId + ".gdl");
            fileChooser.setSelectedFile(file);
            int result = fileChooser.showSaveDialog(controller.getEditorWindow());
            this.guideFile = fileChooser.getSelectedFile();
            if (result == JFileChooser.CANCEL_OPTION) {
                this.guideFile = null;
            } else {
                //All files must end with .gdl
                String absolutePath = this.guideFile.getAbsolutePath();
                if (!absolutePath.toLowerCase().endsWith(".gdl")) {
                    this.guideFile = new File(absolutePath + ".gdl");
                }
                //Set guide Id
                guideId = getGuideIdFromFile(this.guideFile);
                controller.setEntityId(guideId);
            }
        }
        if (this.guideFile != null) {
            //Check guide Id
            String guideId = getGuideIdFromFile(this.guideFile);
            assert controller != null;
            if (!guideId.equals(controller.getEntityId())) {
                int result =
                        JOptionPane.showConfirmDialog(
                                controller.getEditorWindow(),
                                GDLEditorLanguageManager.getMessage("ChangeOfGuideIdFound", new String[]{controller.getEntityId(), guideId}));
                if (result == JOptionPane.CANCEL_OPTION) {
                    this.guideFile = null;
                } else {
                    if (result == JOptionPane.YES_OPTION) {
                        controller.setEntityId(guideId);
                    }
                }
            }
            guideStr = controller.getSerializedEntity();
        }
        if (guideStr != null) {
            windowManager.setBusy(GDLEditorLanguageManager.getMessage("Saving") + "...");
        }
    }

    protected void executeCDSSW() throws InternalErrorException {
        if (guideFile != null && guideStr != null && !guideStr.isEmpty()) {
            try {
                FileOutputStream fos = new FileOutputStream(guideFile);
                OutputStreamWriter output = new OutputStreamWriter(fos, "UTF-8");
                //FileWriter always assumes default encoding is OK!
                output.write(guideStr);
                output.close();
            } catch (IOException ex) {
                throw new InternalErrorException(ex);
            }
        } else {
            this.cancel(true);
        }
    }

    public File getFile() {
        return guideFile;
    }

    private static String getGuideIdFromFile(File guideFile) {
        String guideId = guideFile.getName();
        if (guideId.toLowerCase().endsWith(".gdl")) {
            guideId = guideId.substring(0, guideId.length() - 4);
        }
        return guideId;
    }

    protected void done() {
        try {
            if (guideFile != null && guideStr != null && !guideStr.isEmpty()) {
                controller.entitySaved();
                controller.getEditorFileManager().setLastFileLoaded(guideFile);
                controller.getEditorFileManager().setLastFolderLoaded(guideFile.getParentFile());
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