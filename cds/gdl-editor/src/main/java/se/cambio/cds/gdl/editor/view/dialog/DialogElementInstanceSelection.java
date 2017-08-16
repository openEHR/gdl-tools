package se.cambio.cds.gdl.editor.view.dialog;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.util.NodeDefinitionManager;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeElementInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeInstantiationRuleLine;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.openehr.view.dialogs.DialogSelection;

import javax.swing.*;
import java.awt.*;

public class DialogElementInstanceSelection extends DialogSelection {

    private static final long serialVersionUID = 1L;
    private JButton addArchetypeReferenceButton;
    private GDLEditor controller = null;
    private Object selectedObject = null;
    private boolean onlyCDSDomain;

    public DialogElementInstanceSelection(GDLEditor controller, NodeDefinitionManager nodeDefinitionManager, boolean onlyCDSDomain, ArchetypeReference ar) {
        super(controller.getWindowManager().getMainWindow(),
                GDLEditorLanguageManager.getMessage("SelectElementInstance"),
                nodeDefinitionManager.getElementInstancesSelectionNodes(controller.getDefinitionRuleLines(), onlyCDSDomain, ar),
                true,
                new Dimension(500, 500), controller.getWindowManager());
        this.controller = controller;
        this.onlyCDSDomain = onlyCDSDomain;
        getSelectionPanel().getFilterPanel().add(getAddArchetypeReferenceButton());
    }

    public Object getSelectedObject() {
        if (selectedObject != null) {
            return selectedObject;
        } else {
            return super.getSelectedObject();
        }
    }

    private JButton getAddArchetypeReferenceButton() {
        if (addArchetypeReferenceButton == null) {
            addArchetypeReferenceButton = new JButton();
            addArchetypeReferenceButton.setText(GDLEditorLanguageManager.getMessage("AddArchetype"));
            addArchetypeReferenceButton.setToolTipText(GDLEditorLanguageManager.getMessage("AddArchetypeD"));
            addArchetypeReferenceButton.setIcon(GDLEditorImageUtil.ADD_ICON);
            addArchetypeReferenceButton.setEnabled(true);
            addArchetypeReferenceButton.addActionListener(e -> {
                accept();
                ArchetypeInstantiationRuleLine airl =
                        controller.addArchetypeReference(onlyCDSDomain);
                if (airl != null) {
                    ArchetypeElementInstantiationRuleLine aeirl =
                            controller.addArchetypeElement(airl);
                    if (aeirl != null) {
                        selectedObject = aeirl.getGTCodeRuleLineElement();
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