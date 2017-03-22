package se.cambio.cds.gdl.editor.view.dialog;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.util.NodeDefinitionConversor;
import se.cambio.cds.gdl.model.readable.rule.RuleLineCollection;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeElementInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.GTCodeRuleLineElement;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.view.dialogs.DialogSelection;
import se.cambio.openehr.view.trees.SelectableNode;

import javax.swing.*;
import java.awt.*;

public class DialogElementAttributeFunctionInstanceSelection extends DialogSelection{

    private static final long serialVersionUID = 1L;
    private JButton addArchetypeReferenceButton;
    private GDLEditor controller = null;
    private Object selectedObject = null;
    private boolean onlyCDSDomain;
    private JButton addElementButton;
    private ArchetypeReference archetypeReference;

    public DialogElementAttributeFunctionInstanceSelection(Window owner, GDLEditor controller, boolean onlyCDSDomain, ArchetypeReference ar) {
        super(
                owner,
                GDLEditorLanguageManager.getMessage("SelectElementInstance"),
                NodeDefinitionConversor.getNodeAttributesAndFunctions(controller, onlyCDSDomain, ar),
                true,
                new Dimension(500,500));
        this.controller = controller;
        this.onlyCDSDomain = onlyCDSDomain;
        archetypeReference = ar;
        initButtons();
    }

    private void initButtons(){
        getSelectionPanel().getFilterPanel().add(getAddArchetypeReferenceButton());
        getSelectionPanel().getFilterPanel().add(getAddElementButton());
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
                if (airl!=null){
                    ArchetypeElementInstantiationRuleLine aeirl =
                            controller.addArchetypeElement(airl);
                    if (aeirl!=null){
                        selectAttributeFromGTCodeRLE(aeirl);
                    }
                }
            });
        }
        return addArchetypeReferenceButton;
    }

    private JButton getAddElementButton() {
        if (addElementButton == null) {
            addElementButton = new JButton();
            addElementButton.setText(GDLEditorLanguageManager.getMessage("AddElement"));
            addElementButton.setToolTipText(GDLEditorLanguageManager.getMessage("AddElementD"));
            addElementButton.setIcon(GDLEditorImageUtil.ADD_ICON);
            addElementButton.setEnabled(true);
            addElementButton.addActionListener(e -> addElement());
        }
        return addElementButton;
    }

    private void addElement(){
        GDLEditor controller = EditorManager.getActiveGDLEditor();
        DialogElementInstanceSelection dialog =
                new DialogElementInstanceSelection(EditorManager.getActiveEditorWindow(), controller, false, archetypeReference);
        dialog.setVisible(true);
        if (dialog.getAnswer()){
            Object selectedObject = dialog.getSelectedObject();
            if (selectedObject instanceof ArchetypeInstantiationRuleLine){
                ArchetypeInstantiationRuleLine airl = (ArchetypeInstantiationRuleLine)selectedObject;
                assert controller != null;
                ArchetypeElementInstantiationRuleLine aeirl = controller.addArchetypeElement(airl);
                if (aeirl!=null){
                    selectAttributeFromGTCodeRLE(aeirl);
                }
            }else if (selectedObject instanceof GTCodeRuleLineElement){
                GTCodeRuleLineElement gtCodeRLE = (GTCodeRuleLineElement)selectedObject;
                selectAttributeFromGTCodeRLE((ArchetypeElementInstantiationRuleLine)gtCodeRLE.getParentRuleLine());
            }
        }
    }

    private void selectAttributeFromGTCodeRLE(ArchetypeElementInstantiationRuleLine aeirl){
        RuleLineCollection definitionRuleLines = new RuleLineCollection(aeirl.getReadableGuide());
        definitionRuleLines.add(aeirl);
        SelectableNode<Object> rootNode = NodeDefinitionConversor.getSingleNodeAttributesAndFunctions();
        try {
            NodeDefinitionConversor.addElementInstanceAttributesAndFunctionsToNode(definitionRuleLines, rootNode, onlyCDSDomain, archetypeReference);
        } catch (InstanceNotFoundException e) {
            ExceptionHandler.handle(e);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        }
        DialogSelection dialog =
                new DialogSelection(this, GDLEditorLanguageManager.getMessage("SelectElementInstance"), rootNode);
        dialog.setVisible(true);
        if (dialog.getAnswer()){
            selectedObject = dialog.getSelectedObject();
            accept();
        }
    }

    public Object getSelectedObject(){
        if (selectedObject !=null){
            return selectedObject;
        }else{
            return super.getSelectedObject();
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