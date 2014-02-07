package se.cambio.cds.gdl.editor.view.dialog;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.util.NodeDefinitionConversor;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeElementInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeInstantiationRuleLine;
import se.cambio.openehr.view.dialogs.DialogSelection;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class DialogElementInstanceSelection extends DialogSelection{

    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private JButton addArchetypeReferenceButton;
    private GDLEditor _controller = null;
    private Object _selectedObject = null;
    private boolean _onlyCDSDomain;
    public DialogElementInstanceSelection(Window owner, GDLEditor controller, boolean onlyCDSDomain) {
        super(owner,
                GDLEditorLanguageManager.getMessage("SelectElementInstance"),
                NodeDefinitionConversor.getElementInstancesSelectionNodes(controller.getDefinitionRuleLines(), onlyCDSDomain),
                true,
                new Dimension(500,500));
        _controller = controller;
        _onlyCDSDomain = onlyCDSDomain;
        getSelectionPanel().getFilterPanel().add(getAddArchetypeReferenceButton());
    }

    public Object getSelectedObject(){
        if (_selectedObject!=null){
            return _selectedObject;
        }else{
            return super.getSelectedObject();
        }
    }

    public JButton getAddArchetypeReferenceButton() {
        if (addArchetypeReferenceButton == null) {
            addArchetypeReferenceButton = new JButton();
            addArchetypeReferenceButton.setText(GDLEditorLanguageManager.getMessage("AddArchetype"));
            addArchetypeReferenceButton.setToolTipText(GDLEditorLanguageManager.getMessage("AddArchetypeD"));
            addArchetypeReferenceButton.setIcon(GDLEditorImageUtil.ADD_ICON);
            addArchetypeReferenceButton.setEnabled(true);
            addArchetypeReferenceButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    accept();
                    ArchetypeInstantiationRuleLine airl =
                            _controller.addArchetypeReference(_onlyCDSDomain);
                    if (airl!=null){
                        ArchetypeElementInstantiationRuleLine aeirl =
                                _controller.addArchetypeElement(airl);
                        if (aeirl!=null){
                            _selectedObject = aeirl.getGTCodeRuleLineElement();
                        }
                    }
                }
            });
        }
        return addArchetypeReferenceButton;
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