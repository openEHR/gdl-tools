/*
 * Created on 30-ago-2006
 *


 */
package se.cambio.cds.gdl.editor.view.menubar;

import lombok.extern.slf4j.Slf4j;
import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.util.export.html.GuideHTMLExporter;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

@Slf4j
public class ExportToHTMLAction extends AbstractAction {

    private static final long serialVersionUID = -3561842193285119707L;
    private GuideHTMLExporter guideHTMLExporter;
    private EditorManager editorManager;

    ExportToHTMLAction(GuideHTMLExporter guideHTMLExporter, EditorManager editorManager) {
        super();
        this.guideHTMLExporter = guideHTMLExporter;
        this.editorManager = editorManager;
        putValue(NAME, GDLEditorLanguageManager.getMessage("ExportToHTML") + "...");
        putValue(SMALL_ICON, null);
        putValue(SHORT_DESCRIPTION, GDLEditorLanguageManager.getMessage("ExportToHTMLD"));
        putValue(LONG_DESCRIPTION, GDLEditorLanguageManager.getMessage("ExportToHTMLD"));
    }

    public void actionPerformed(ActionEvent ev) {
        GDLEditor controller = editorManager.getActiveGDLEditor();
        Guide guide = controller.getEntity();
        exportToHTML(editorManager.getActiveEditorWindow(), guide, controller.getCurrentLanguageCode(), guideHTMLExporter);
    }

    private void exportToHTML(Window owner, Guide guide, String lang, GuideHTMLExporter guideHTMLExporter) {
        JFileChooser fileChooser = new JFileChooser();
        FileNameExtensionFilter filter = new FileNameExtensionFilter("HTML", "html");
        fileChooser.setDialogTitle(OpenEHRLanguageManager.getMessage("ExportToHTML"));
        fileChooser.setFileFilter(filter);
        File selectedFile = new File(guide.getId() + ".html");
        fileChooser.setSelectedFile(selectedFile);
        int result = fileChooser.showSaveDialog(owner);
        if (result != JFileChooser.CANCEL_OPTION) {
            try {
                selectedFile = fileChooser.getSelectedFile();
                FileWriter fstream = new FileWriter(selectedFile);
                BufferedWriter out = new BufferedWriter(fstream);
                out.write(guideHTMLExporter.convertToHTML(guide, lang));
                out.close();
            } catch (IOException | InternalErrorException ex) {
                log.error("Error exporting to HTML guideline: " + guide.getId(), ex);
            }
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