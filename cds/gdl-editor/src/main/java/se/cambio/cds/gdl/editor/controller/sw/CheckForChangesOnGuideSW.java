package se.cambio.cds.gdl.editor.controller.sw;

import lombok.extern.slf4j.Slf4j;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.panels.GDLPanel;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.util.CDSSwingWorker;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import java.awt.*;
import java.io.ByteArrayInputStream;

@Slf4j
public class CheckForChangesOnGuideSW extends CDSSwingWorker {
    private GDLEditor controller = null;

    public CheckForChangesOnGuideSW(GDLEditor controller) {
        this.controller = controller;
    }

    @Override
    protected void executeCDSSW() throws InternalErrorException {
        try {
            while (controller.isActive()) {
                SwingUtilities.invokeLater(() -> {
                    checkGuideConsistency();
                    JButton saveButton = controller.getEditorPanel().getSaveButton();
                    saveButton.setEnabled(controller.isModified());
                    saveButton.repaint();
                    saveButton.revalidate();
                });
                Thread.sleep(1000);
            }
        } catch (InterruptedException ex) {
            throw new InternalErrorException(ex);
        }
    }

    private void checkGuideConsistency() {
        Component selectedTab = controller.getEditorPanel().getGuidePanel().getGuideEditorTabPane().getSelectedComponent();
        if (selectedTab instanceof GDLPanel) {
            GDLPanel gdlPanel = (GDLPanel) selectedTab;
            Guide guide = null;
            try {
                ByteArrayInputStream bais = new ByteArrayInputStream(gdlPanel.getGuideStr().getBytes("UTF-8"));
                guide = GuideUtil.parseGuide(bais);
                String statusMsg = GDLEditorLanguageManager.getMessage("ParsedCorrectly");
                gdlPanel.setStatus(GDLPanel.StatusType.CORRECT, statusMsg);
                controller.updateGuide(guide);
            } catch (Exception ex) {
                String statusMsg = GDLEditorLanguageManager.getMessage("ErrorParsingGuide") + ": " + ex.getMessage();
                gdlPanel.setStatus(GDLPanel.StatusType.ERRONEOUS, statusMsg);
                log.error("Error parsing guideline", ex);
            }
            controller.setOnlyGDLSourceEditing(guide == null);
        }
    }

    protected void done() {
        JButton saveButton = controller.getEditorPanel().getSaveButton();
        saveButton.setEnabled(true);
        saveButton.repaint();
        saveButton.revalidate();
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