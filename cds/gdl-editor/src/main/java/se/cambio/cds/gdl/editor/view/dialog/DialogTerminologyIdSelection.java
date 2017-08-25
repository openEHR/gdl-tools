package se.cambio.cds.gdl.editor.view.dialog;

import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.util.NodeDefinitionManager;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.view.dialogs.DialogSelection;
import se.cambio.openehr.view.util.WindowManager;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

public class DialogTerminologyIdSelection extends DialogSelection {

    private static final long serialVersionUID = 1L;
    private JButton addTerminologyButton;
    private String terminologyIdCreated = null;

    public DialogTerminologyIdSelection(WindowManager windowManager, List<String> terminologyIds) {
        super(
                windowManager.getMainWindow(),
                GDLEditorLanguageManager.getMessage("AddTerminologyDesc"),
                NodeDefinitionManager.getNodeTerminologyIds(terminologyIds),
                true,
                new Dimension(500, 500), windowManager);
        getSelectionPanel().getFilterPanel().add(getAddTerminologyButton());
    }

    private JButton getAddTerminologyButton() {
        if (addTerminologyButton == null) {
            addTerminologyButton = new JButton(GDLEditorLanguageManager.getMessage("AddTerminology"));
            addTerminologyButton.setIcon(OpenEHRImageUtil.ADD_ICON);
            addTerminologyButton.addActionListener(new AddTerminologyActionListener(this));
        }
        return addTerminologyButton;
    }

    public String getSelectedObject() {
        if (terminologyIdCreated != null) {
            return terminologyIdCreated;
        } else {
            return (String) super.getSelectedObject();
        }
    }

    private boolean isValidTerminologyId(String value) {
        if (value.isEmpty()) {
            return false;
        }
        for (Character character : value.toCharArray()) {
            if (!Character.isJavaIdentifierPart(character)) {
                return false;
            }
        }
        return true;
    }

    private class AddTerminologyActionListener implements ActionListener {
        private JDialog _dialog = null;

        AddTerminologyActionListener(JDialog dialog) {
            _dialog = dialog;
        }

        public void actionPerformed(ActionEvent ev) {
            boolean correctName;
            do {
                correctName = true;
                DialogNameInsert dialog = new DialogNameInsert(_dialog, GDLEditorLanguageManager.getMessage("AddTerminologyDesc"), null);
                if (dialog.getAnswer()) {
                    String value = dialog.getValue();
                    correctName = isValidTerminologyId(value);
                    if (correctName) {
                        terminologyIdCreated = value;
                        accept();
                    } else {
                        JOptionPane.showMessageDialog(
                                _dialog, GDLEditorLanguageManager.getMessage("InvalidId"),
                                GDLEditorLanguageManager.getMessage("InvalidId"),
                                JOptionPane.ERROR_MESSAGE);
                    }
                }
            }
            while (!correctName);
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