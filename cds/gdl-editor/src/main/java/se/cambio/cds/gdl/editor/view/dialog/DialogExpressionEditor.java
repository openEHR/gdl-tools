
package se.cambio.cds.gdl.editor.view.dialog;

import org.slf4j.LoggerFactory;
import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.util.AttributeFunctionContainerNode;
import se.cambio.cds.gdl.editor.view.util.NodeDefinitionConversor;
import se.cambio.cds.gdl.model.expression.AssignmentExpression;
import se.cambio.cds.gdl.model.expression.ExpressionItem;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeElementInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ExpressionRuleLineElement;
import se.cambio.cds.gdl.model.readable.util.ExpressionUtil;
import se.cambio.cds.gdl.parser.ExpressionParser;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.view.dialogs.DialogEditor;
import se.cambio.openehr.view.panels.SelectionPanel;
import se.cambio.openehr.view.trees.SelectableNode;
import se.cambio.openehr.view.trees.SelectableNodeBuilder;
import se.cambio.openehr.view.util.NodeConversor;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.StyledEditorKit;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.StringReader;

public class DialogExpressionEditor extends DialogEditor {

    private static final long serialVersionUID = -2770907170844293126L;
    private JPanel buttonsPanel = null;
    private JPanel mainPanel;
    private ExpressionRuleLineElement expressionRuleLineElement = null;
    private SelectionPanel selectionPanel;
    private JPanel expressionEditorPanel;
    private JPanel renderedExpressionPanel;
    private JEditorPane renderedExpressionTextComponent;
    private JTextArea expressionEditorTextComponent;
    private ExpressionItem _expressionItem = null;
    private JButton addElementButton;
    private boolean inPredicate;
    private ArchetypeReference archetypeReference;

    public DialogExpressionEditor(Window owner, ExpressionRuleLineElement expressionRuleLineElement, boolean inPredicate, ArchetypeReference ar) {
        super(owner, GDLEditorLanguageManager.getMessage("ExpressionEditor"), new Dimension(700, 400), true, true);
        this.expressionRuleLineElement = expressionRuleLineElement;
        if (this.expressionRuleLineElement.getValue() != null) {
            _expressionItem = this.expressionRuleLineElement.getValue();
            getExpressionEditorTextComponent().setText(ExpressionUtil.getEditableExpressionString(_expressionItem));
        }
        this.inPredicate = inPredicate;
        archetypeReference = ar;
        initialize();
    }

    private void initialize() {
        getJPanel().setLayout(new BorderLayout());
        getJPanel().add(getMainPanel(), BorderLayout.CENTER);
        getJPanel().add(getButtonsPanel(), BorderLayout.SOUTH);
    }


    private JPanel getButtonsPanel() {
        if (buttonsPanel == null) {
            buttonsPanel = new JPanel(new BorderLayout());
            JPanel panelAux = new JPanel(new FlowLayout(FlowLayout.CENTER));
            panelAux.add(getAcceptButton());
            panelAux.add(getCancelButton());
            buttonsPanel.add(panelAux, BorderLayout.CENTER);
            if (!inPredicate) {
                panelAux = new JPanel(new FlowLayout(FlowLayout.CENTER));
                panelAux.add(getAddElementButton());
                buttonsPanel.add(panelAux, BorderLayout.EAST);
            }
        }
        return buttonsPanel;
    }

    private JPanel getMainPanel() {
        if (mainPanel == null) {
            mainPanel = new JPanel(new BorderLayout());
            JPanel expressionPanel = new JPanel(new BorderLayout());
            expressionPanel.add(getExpressionEditorPanel(), BorderLayout.CENTER);
            expressionPanel.add(getRenderedExpressionPanel(), BorderLayout.SOUTH);
            mainPanel.add(expressionPanel, BorderLayout.CENTER);
            mainPanel.add(getSelectionPanel(), BorderLayout.EAST);
        }
        return mainPanel;
    }

    private SelectionPanel getSelectionPanel() {
        if (selectionPanel == null) {
            selectionPanel = new SelectionPanel(new SelectableNodeBuilder().createSelectableNode());
            selectionPanel.setPreferredSize(new Dimension(300, 600));
            updateSelectionPanel();
        }
        return selectionPanel;
    }

    private void updateSelectionPanel() {
        SelectableNode<Object> node;
        if (inPredicate) {
            node = NodeDefinitionConversor.getNodeAttributesAndFunctionsPredicate();
        } else {
            node = NodeDefinitionConversor.getNodeAttributesAndFunctions(EditorManager.getActiveGDLEditor(), false, archetypeReference);
        }
        selectionPanel.changeRootNode(node);
        selectionPanel.getJTree().expand(node);
        selectionPanel.getJTree().addExtraMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.getClickCount() == 2) {
                    Object obj = NodeConversor.getSelectedElement(getSelectionPanel().getNode());
                    if (obj instanceof AttributeFunctionContainerNode) {
                        AttributeFunctionContainerNode attNode = (AttributeFunctionContainerNode) obj;
                        String handle = "$" + attNode.getGtCodeRuleLineElement().getValue() + "." + attNode.getAttributeFunction();
                        getExpressionEditorTextComponent().replaceSelection(handle);
                    }
                }
            }
        });
    }

    private JPanel getRenderedExpressionPanel() {
        if (renderedExpressionPanel == null) {
            renderedExpressionPanel = new JPanel(new BorderLayout());
            renderedExpressionPanel.setBorder(BorderFactory.createTitledBorder(GDLEditorLanguageManager.getMessage("ExpressionViewer")));
            JScrollPane jScrollPane = new JScrollPane();
            jScrollPane.setViewportView(getRenderedExpressionTextComponent());
            jScrollPane.setPreferredSize(new Dimension(300, 150));
            renderedExpressionPanel.add(jScrollPane, BorderLayout.CENTER);
        }
        return renderedExpressionPanel;
    }

    private JEditorPane getRenderedExpressionTextComponent() {
        if (renderedExpressionTextComponent == null) {
            renderedExpressionTextComponent = new JEditorPane();
            renderedExpressionTextComponent.setEditorKit(new StyledEditorKit());
            renderedExpressionTextComponent.setContentType("text/html");
            renderedExpressionTextComponent.setEditable(false);
        }
        return renderedExpressionTextComponent;
    }

    private JPanel getExpressionEditorPanel() {
        if (expressionEditorPanel == null) {
            expressionEditorPanel = new JPanel(new BorderLayout());
            expressionEditorPanel.setBorder(BorderFactory.createTitledBorder(GDLEditorLanguageManager.getMessage("ExpressionEditor")));
            JScrollPane jScrollPane = new JScrollPane();
            jScrollPane.setViewportView(getExpressionEditorTextComponent());
            expressionEditorPanel.add(jScrollPane, BorderLayout.CENTER);
        }
        return expressionEditorPanel;
    }

    private JTextArea getExpressionEditorTextComponent() {
        if (expressionEditorTextComponent == null) {
            expressionEditorTextComponent = new JTextArea();
            expressionEditorTextComponent.setLineWrap(true);
            expressionEditorTextComponent.getDocument().addDocumentListener(new DocumentListener() {
                @Override
                public void removeUpdate(DocumentEvent e) {
                    update();
                }

                @Override
                public void insertUpdate(DocumentEvent e) {
                    update();
                }

                @Override
                public void changedUpdate(DocumentEvent e) {
                    update();
                }

                private void update() {
                    updateRenderedTextComponent(getExpressionEditorTextComponent().getText());
                }
            });
        }
        return expressionEditorTextComponent;
    }

    private void updateRenderedTextComponent(String expression) {
        _expressionItem = null;
        try {
            _expressionItem = ((AssignmentExpression) parse(expression)).getAssignment();
        } catch (Throwable e) {
            LoggerFactory.getLogger(DialogExpressionEditor.class).warn("Error parsing expression: " + e.getMessage());
        }
        if (_expressionItem != null) {
            String htmlStr = ExpressionUtil.convertToHTMLText(expressionRuleLineElement, _expressionItem, UserConfigurationManager.instance().getLanguage());
            getRenderedExpressionTextComponent().setText(htmlStr);
        } else {
            getRenderedExpressionTextComponent().setText("");
        }
        getExpressionEditorTextComponent().requestFocus();
    }

    public ExpressionItem getExpressionItem() {
        return _expressionItem;
    }

    private static ExpressionItem parse(String value) throws Exception {
        value = value.trim();
        if (!value.startsWith("(") || !value.endsWith(")")) {
            value = "(" + value + ")";
        }
        value = "$gt0001.value=" + value;
        ExpressionParser parser = new ExpressionParser(new StringReader(value));
        return parser.parse();
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

    private void addElement() {
        GDLEditor controller = EditorManager.getActiveGDLEditor();
        DialogElementInstanceSelection dialog =
                new DialogElementInstanceSelection(EditorManager.getActiveEditorWindow(), controller, false, archetypeReference);
        dialog.setVisible(true);
        if (dialog.getAnswer()) {
            Object selectedObject = dialog.getSelectedObject();
            if (selectedObject instanceof ArchetypeInstantiationRuleLine) {
                ArchetypeInstantiationRuleLine airl = (ArchetypeInstantiationRuleLine) selectedObject;
                assert controller != null;
                ArchetypeElementInstantiationRuleLine aeirl = controller.addArchetypeElement(airl);
                if (aeirl != null) {
                    updateSelectionPanel();
                }
            } else {
                updateSelectionPanel();
            }
        }
    }

    protected boolean acceptDialog() {
        if (_expressionItem != null) {
            return true;
        } else {
            JOptionPane.showMessageDialog(this, GDLEditorLanguageManager.getMessage("EmptyExpressionErrorMsg"), GDLEditorLanguageManager.getMessage("EmptyExpressionErrorTitle"), JOptionPane.ERROR_MESSAGE);
            return false;
        }
    }

}  //  @jve:decl-index=0:visual-constraint="124,21"
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