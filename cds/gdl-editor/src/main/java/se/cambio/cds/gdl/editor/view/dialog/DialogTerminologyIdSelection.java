package se.cambio.cds.gdl.editor.view.dialog;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.util.NodeDefinitionConversor;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.view.dialogs.DialogSelection;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class DialogTerminologyIdSelection extends DialogSelection{
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private JButton addTerminologyButton;
    private String _terminologyIdCreated = null;

    public DialogTerminologyIdSelection(Window owner, GDLEditor controller) {
        super(
                owner,
                GDLEditorLanguageManager.getMessage("AddTerminologyDesc"),
                NodeDefinitionConversor.getNodeTerminologyIds(),
                true,
                new Dimension(500,500));
        getSelectionPanel().getFilterPanel().add(getAddTerminologyButton());
    }

    private JButton getAddTerminologyButton(){
        if (addTerminologyButton==null){
            addTerminologyButton = new JButton(GDLEditorLanguageManager.getMessage("AddTerminology"));
            addTerminologyButton.setIcon(OpenEHRImageUtil.ADD_ICON);
            addTerminologyButton.addActionListener(new AddTerminologyActionListener(this));
        }
        return addTerminologyButton;
    }

    public String getSelectedObject(){
        if (_terminologyIdCreated!=null){
            return _terminologyIdCreated;
        }else{
            return (String)super.getSelectedObject();
        }
    }

    private class AddTerminologyActionListener implements ActionListener{
        private JDialog _dialog = null;
        public AddTerminologyActionListener(JDialog dialog){
            _dialog = dialog;
        }
        public void actionPerformed(ActionEvent e) {
            DialogNameInsert dialog = new DialogNameInsert(_dialog, GDLEditorLanguageManager.getMessage("AddTerminologyDesc"), null);
            if (dialog.getAnswer()){
                _terminologyIdCreated = dialog.getValue();
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