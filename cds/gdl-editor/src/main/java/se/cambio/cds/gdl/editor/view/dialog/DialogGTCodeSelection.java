package se.cambio.cds.gdl.editor.view.dialog;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.util.NodeDefinitionManager;
import se.cambio.cds.gdl.model.Term;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.view.dialogs.DialogSelection;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Map;

public class DialogGTCodeSelection extends DialogSelection {

    private static final long serialVersionUID = 1L;
    private JButton addLocalTermButton;
    private String codeCreated = null;
    private GDLEditor controller;

    public DialogGTCodeSelection(GDLEditor controller) {
        super(
                controller.getEditorWindow(),
                GDLEditorLanguageManager.getMessage("SelectLocalTerm"),
                NodeDefinitionManager.getNodeGTCodes(controller.getCurrentTermsMap(), controller.getGTCodesUsedInDefinitions()),
                true,
                new Dimension(500, 500), controller.getWindowManager());
        this.controller = controller;
        getSelectionPanel().getFilterPanel().add(getAddLocalTermButton());
    }

    private JButton getAddLocalTermButton() {
        if (addLocalTermButton == null) {
            addLocalTermButton = new JButton(GDLEditorLanguageManager.getMessage("AddLocalTerm"));
            addLocalTermButton.setIcon(OpenEHRImageUtil.ADD_ICON);
            addLocalTermButton.addActionListener(new AddTermActionListener(this));
        }
        return addLocalTermButton;
    }

    public String getSelectedObject() {
        if (codeCreated != null) {
            return codeCreated;
        } else {
            return (String) super.getSelectedObject();
        }
    }

    private class AddTermActionListener implements ActionListener {
        private JDialog _dialog = null;

        AddTermActionListener(JDialog dialog) {
            _dialog = dialog;
        }

        public void actionPerformed(ActionEvent ev) {
            DialogNameInsert dialog = new DialogNameInsert(_dialog, GDLEditorLanguageManager.getMessage("AddLocalTerm"), null);
            if (dialog.getAnswer()) {
                codeCreated = controller.createNextLocalCode();
                Map<String, Term> currentTermsMap = controller.getCurrentTermsMap();
                currentTermsMap.get(codeCreated).setText(dialog.getValue());
                accept();
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