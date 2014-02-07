package se.cambio.cds.gdl.editor.view.dialog;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.util.NodeDefinitionConversor;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeElementInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.GTCodeRuleLineElement;
import se.cambio.openehr.view.dialogs.DialogSelection;
import se.cambio.openehr.view.trees.SelectableNode;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;

public class DialogElementAttributeFunctionInstanceSelection extends DialogSelection{

    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private JButton addArchetypeReferenceButton;
    private GDLEditor _controller = null;
    private Object _selectedObject = null;
    private boolean _onlyCDSDomain;
    private JButton addElementButton;

    public DialogElementAttributeFunctionInstanceSelection(Window owner, GDLEditor controller, boolean onlyCDSDomain) {
        super(
                owner,
                GDLEditorLanguageManager.getMessage("SelectElementInstance"),
                NodeDefinitionConversor.getNodeAttributesAndFunctions(controller.getDefinitionRuleLines(), onlyCDSDomain),
                true,
                new Dimension(500,500));
        _controller = controller;
        _onlyCDSDomain = onlyCDSDomain;
        initButtons();
    }

    public void initButtons(){
        getSelectionPanel().getFilterPanel().add(getAddArchetypeReferenceButton());
        getSelectionPanel().getFilterPanel().add(getAddElementButton());
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
                            selectAttributeFromGTCodeRLE(aeirl);
                        }
                    }
                }
            });
        }
        return addArchetypeReferenceButton;
    }

    protected JButton getAddElementButton() {
        if (addElementButton == null) {
            addElementButton = new JButton();
            addElementButton.setText(GDLEditorLanguageManager.getMessage("AddElement"));
            addElementButton.setToolTipText(GDLEditorLanguageManager.getMessage("AddElementD"));
            addElementButton.setIcon(GDLEditorImageUtil.ADD_ICON);
            addElementButton.setEnabled(true);
            addElementButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    addElement();
                }
            });
        }
        return addElementButton;
    }

    public void addElement(){
        GDLEditor controller = EditorManager.getActiveGDLEditor();
        DialogElementInstanceSelection dialog =
                new DialogElementInstanceSelection(EditorManager.getActiveEditorWindow(), controller, false);
        dialog.setVisible(true);
        if (dialog.getAnswer()){
            Object selectedObject = dialog.getSelectedObject();
            if (selectedObject instanceof ArchetypeInstantiationRuleLine){
                ArchetypeInstantiationRuleLine airl = (ArchetypeInstantiationRuleLine)selectedObject;
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

    /*
    private void updateSelectionPanel(){
	SelectableNode<Object> node = 
		NodeDefinitionConversor.getNodeAttributesAndFunctions(EditorManager.getActiveGDLEditor().getDefinitionRuleLines(), false);
	this.setRootNode(node, true);
	initButtons();
	getSelectionPanel().revalidate();
	getSelectionPanel().repaint();
    }*/

    private void selectAttributeFromGTCodeRLE(ArchetypeElementInstantiationRuleLine aeirl){
        Collection<RuleLine> definitionRuleLines = new ArrayList<RuleLine>();
        definitionRuleLines.add(aeirl);
        SelectableNode<Object> rootNode = NodeDefinitionConversor.getSingleNodeAttributesAndFunctions();
        NodeDefinitionConversor.addElementInstanceAttributesAndFunctionsToNode(definitionRuleLines, rootNode, _onlyCDSDomain);
        DialogSelection dialog =
                new DialogSelection(this, GDLEditorLanguageManager.getMessage("SelectElementInstance"), rootNode);
        dialog.setVisible(true);
        if (dialog.getAnswer()){
            _selectedObject = dialog.getSelectedObject();
            accept();
        }
    }

    public Object getSelectedObject(){
        if (_selectedObject!=null){
            return _selectedObject;
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