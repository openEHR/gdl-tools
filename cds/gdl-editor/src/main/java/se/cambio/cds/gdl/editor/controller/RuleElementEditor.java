package se.cambio.cds.gdl.editor.controller;

import se.cambio.cds.controller.session.data.ArchetypeReferencesManager;
import se.cambio.cds.gdl.editor.util.DefinitionDependencyChecker;
import se.cambio.cds.gdl.editor.view.dialog.*;
import se.cambio.cds.gdl.editor.view.panels.DVLocalCodedTextPanel;
import se.cambio.cds.gdl.editor.view.util.AttributeFunctionContainerNode;
import se.cambio.cds.gdl.editor.view.util.NodeDefinitionManager;
import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.expression.ExpressionItem;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.gdl.model.expression.StringConstant;
import se.cambio.cds.gdl.model.readable.rule.lines.*;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.*;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ActionRuleLine;
import se.cambio.cds.gdl.model.readable.util.PredicateAttributeVO;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.view.swing.dialogs.DialogArchetypeChooser;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.cm.model.template.dto.TemplateDTO;
import se.cambio.cm.model.util.CMElement;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.view.dialogs.DVDialogEditor;
import se.cambio.openehr.view.dialogs.DialogEditorNullValue;
import se.cambio.openehr.view.trees.SelectableNode;
import se.cambio.openehr.view.util.DVPanelFactory;
import se.cambio.openehr.view.util.WindowManager;

import java.awt.*;
import java.util.Collection;

class RuleElementEditor {

    private ArchetypeReferencesManager archetypeReferencesManager;
    private ArchetypeManager archetypeManager;
    private DVPanelFactory dvPanelFactory;
    private NodeDefinitionManager nodeDefinitionManager;
    private GDLEditor gdlEditor;
    private WindowManager windowManager;

    RuleElementEditor(
            WindowManager windowManager,
            ArchetypeReferencesManager archetypeReferencesManager,
            ArchetypeManager archetypeManager,
            DVPanelFactory dvPanelFactory,
            GDLEditor gdlEditor) {
        this.windowManager = windowManager;
        this.archetypeReferencesManager = archetypeReferencesManager;
        this.archetypeManager = archetypeManager;
        this.dvPanelFactory = dvPanelFactory;
        this.gdlEditor = gdlEditor;
    }

    private NodeDefinitionManager getNodeDefinitionManager() {
        if (nodeDefinitionManager == null) {
            nodeDefinitionManager = new NodeDefinitionManager(archetypeReferencesManager, gdlEditor);
        }
        return nodeDefinitionManager;
    }

    void edit(RuleLineElementWithValue<?> ruleLineElementWithValue) throws InternalErrorException, InstanceNotFoundException {
        if (ruleLineElementWithValue instanceof ArchetypeReferenceRuleLineDefinitionElement) {
            editArchetype((ArchetypeReferenceRuleLineDefinitionElement) ruleLineElementWithValue);
        } else if (ruleLineElementWithValue instanceof GTCodeRuleLineElement) {
            rename((GTCodeRuleLineElement) ruleLineElementWithValue);
        } else if (ruleLineElementWithValue instanceof ArchetypeElementRuleLineDefinitionElement) {
            editArchetypeElement((ArchetypeElementRuleLineDefinitionElement) ruleLineElementWithValue);
        } else if (ruleLineElementWithValue instanceof CDSEntryRuleLineElement) {
            selectCDSEntry((CDSEntryRuleLineElement) ruleLineElementWithValue);
        } else if (ruleLineElementWithValue instanceof ArchetypeElementRuleLineElement) {
            selectArchetypeElement((ArchetypeElementRuleLineElement) ruleLineElementWithValue);
        } else if (ruleLineElementWithValue instanceof DataValueRuleLineElement) {
            editDataValue((DataValueRuleLineElement) ruleLineElementWithValue);
        } else if (ruleLineElementWithValue instanceof NullValueRuleLineElement) {
            editNullValue((NullValueRuleLineElement) ruleLineElementWithValue);
        } else if (ruleLineElementWithValue instanceof PredicateArchetypeElementAttributeRuleLineElement) {
            editPredicateAttribute((PredicateArchetypeElementAttributeRuleLineElement) ruleLineElementWithValue);
        } else if (ruleLineElementWithValue instanceof ArchetypeElementAttributeRuleLineElement) {
            editAttribute((ArchetypeElementAttributeRuleLineElement) ruleLineElementWithValue);
        } else if (ruleLineElementWithValue instanceof ExpressionRuleLineElement) {
            editExpression((ExpressionRuleLineElement) ruleLineElementWithValue);
        } else if (ruleLineElementWithValue instanceof FiredRuleReferenceRuleElement) {
            selectFiredRule((FiredRuleReferenceRuleElement) ruleLineElementWithValue);
        }
    }

    private void editArchetype(ArchetypeReferenceRuleLineDefinitionElement arrlde) {
        ArchetypeInstantiationRuleLine airl = (ArchetypeInstantiationRuleLine) arrlde.getParentRuleLine();
        boolean showOnlyCDS = DefinitionDependencyChecker.isBeingUsedInAction(airl, gdlEditor);
        editArchetype(arrlde, showOnlyCDS);
    }

    void editArchetype(ArchetypeReferenceRuleLineDefinitionElement arrlde, boolean showOnlyCDS) {
        Window owner = gdlEditor.getEditorWindow();
        ArchetypeReference ar = arrlde.getValue();
        String idArchetype = ar != null ? ar.getIdArchetype() : null;
        String idTemplate = ar != null ? ar.getIdTemplate() : null;
        boolean isTemplate = idTemplate != null;
        String domainId = ar != null ? ar.getIdDomain() : null;
        DialogArchetypeChooser dialog = new DialogArchetypeChooser(owner, idArchetype, domainId, isTemplate, showOnlyCDS);
        dialog.setVisible(true);
        if (dialog.getAnswer()) {
            CMElement cmElement = dialog.getSelectedCMElement();
            if (cmElement instanceof ArchetypeDTO) {
                idArchetype = cmElement.getId();
            } else if (cmElement instanceof TemplateDTO) {
                idArchetype = ((TemplateDTO) cmElement).getArchetypeId();
                idTemplate = cmElement.getId();
            }
            if (idArchetype == null) {
                if (ar != null) {
                    idArchetype = ar.getIdArchetype();
                    idTemplate = ar.getIdTemplate();
                }
            }
            if (idArchetype != null) {
                String idDomain = dialog.getSelectedDomain();
                ar = new ArchetypeReference(idDomain, idArchetype, idTemplate);
                arrlde.setValue(ar);
            }
        }
    }

    private void rename(GTCodeRuleLineElement gtCodeRuleLineElement) {
        Window owner = gdlEditor.getEditorWindow();
        String title = null;
        RuleLine ruleLine = gtCodeRuleLineElement.getParentRuleLine();
        if (ruleLine instanceof ArchetypeInstantiationRuleLine) {
            title = ((ArchetypeInstantiationRuleLine) ruleLine).getArchetypeReferenceRuleLineDefinitionElement().getLabelText();
        } else if (ruleLine instanceof ArchetypeElementInstantiationRuleLine) {
            title = ((ArchetypeElementInstantiationRuleLine) ruleLine).getArchetypeElementRuleLineDefinitionElement().getLabelText();
        }
        Term term = gdlEditor.getTerm(gtCodeRuleLineElement.getValue());
        String oldValue = term.getText();
        DialogNameInsert dialog = new DialogNameInsert(owner, title, oldValue);
        if (dialog.getAnswer()) {
            term.setText(dialog.getValue());
        }
    }

    private void editArchetypeElement(ArchetypeElementRuleLineDefinitionElement aerlde) throws InstanceNotFoundException, InternalErrorException {
        ArchetypeReference ar = aerlde.getArchetypeReference();
        if (ar != null) {
            SelectableNode<Object> node =
                    NodeDefinitionManager.getElementsInArchetypeNode(ar.getIdArchetype(), ar.getIdTemplate(), aerlde.getArchetypeManager());
            DialogElementSelection dialog = new DialogElementSelection(gdlEditor.getWindowManager(), node);
            dialog.setVisible(true);
            if (dialog.getAnswer()) {
                Object obj = dialog.getSelectedObject();
                if (obj instanceof ArchetypeElementVO) {
                    aerlde.setValue((ArchetypeElementVO) obj);
                    RuleLine ruleLine = aerlde.getParentRuleLine();
                    if (ruleLine instanceof ArchetypeElementInstantiationRuleLine) {
                        ArchetypeElementInstantiationRuleLine aeirl =
                                (ArchetypeElementInstantiationRuleLine) ruleLine;
                        String gtCode = aeirl.getGTCodeRuleLineElement().getValue();
                        Term term = gdlEditor.getTerm(gtCode);
                        if (term.getText() == null || term.getText().isEmpty()) {
                            String name = aerlde.getArchetypeManager().getArchetypeElements().getText(aerlde.getValue(), UserConfigurationManager.instance().getLanguage());
                            String desc = aerlde.getArchetypeManager().getArchetypeElements().getDescription(aerlde.getValue(), UserConfigurationManager.instance().getLanguage());
                            term.setText(name);
                            term.setDescription(desc);
                        }
                    }
                }
            }
        }
    }

    private void selectArchetypeElement(ArchetypeElementRuleLineElement arrle) {
        boolean onlyCDSDomain = onlyCDSDomain(arrle);
        Object selectedObject = null;
        ArchetypeReference ar = getArchetypeReferenceFromCreateInstanceRuleLine(arrle, onlyCDSDomain);
        DialogElementInstanceSelection dialog =
                new DialogElementInstanceSelection(gdlEditor, getNodeDefinitionManager(), onlyCDSDomain, ar);
        dialog.setVisible(true);
        if (dialog.getAnswer()) {
            selectedObject = dialog.getSelectedObject();
        }
        if (selectedObject instanceof GTCodeRuleLineElement) {
            GTCodeRuleLineElement gtCodeRuleLineElement = (GTCodeRuleLineElement) selectedObject;
            if (gtCodeRuleLineElement.getParentRuleLine() instanceof ArchetypeElementInstantiationRuleLine) {
                arrle.setValue(gtCodeRuleLineElement);
            }
        } else if (selectedObject instanceof ArchetypeInstantiationRuleLine) {
            ArchetypeInstantiationRuleLine airl = (ArchetypeInstantiationRuleLine) selectedObject;
            ArchetypeElementInstantiationRuleLine aeirl = gdlEditor.addArchetypeElement(airl);
            if (aeirl != null) {
                arrle.setValue(aeirl.getGTCodeRuleLineElement());
            }
        }
    }

    private static ArchetypeReference getArchetypeReferenceFromCreateInstanceRuleLine(RuleLineElementWithValue<?> ruleLineElement, boolean onlyCDSDomain) {
        //If the set is inside a create instance action, we limit the selection to the created archetype reference
        if (onlyCDSDomain && ruleLineElement.getParentRuleLine().getParentRuleLine() instanceof CreateInstanceActionRuleLine) {
            return ((CreateInstanceActionRuleLine) ruleLineElement.getParentRuleLine().getParentRuleLine()).getArchetypeReference();
        } else {
            return null;
        }
    }

    //This method creates a dialog, if the user selects a valid object (ArchetypeInstantiationRuleLine)
    // it sets the referring element's value (CDSEntryRuleLineElement) to the GTCode of the archetype instance.
    private void selectCDSEntry(CDSEntryRuleLineElement cdserle) {
        Object selectedObject = null;
        //if (!emptyDefinitions(controller.getDefinitionRuleLines(), onlyCDSDomain)){
        DialogEntrySelection dialog =
                new DialogEntrySelection(gdlEditor, true);
        dialog.setVisible(true);
        if (dialog.getAnswer()) {
            selectedObject = dialog.getSelectedObject();
        }

        if (selectedObject instanceof ArchetypeInstantiationRuleLine) {
            ArchetypeInstantiationRuleLine airl = (ArchetypeInstantiationRuleLine) selectedObject;
            cdserle.setValue(airl.getGTCodeRuleLineElement());
        }
    }

    private void selectFiredRule(FiredRuleReferenceRuleElement frrrl) {
        DialogRuleSelection dialog = new DialogRuleSelection(gdlEditor);
        dialog.setVisible(true);
        if (dialog.getAnswer()) {
            GTCodeRuleLineElement gtCodeRuleLineElement = dialog.getSelectedGTCodeRuleLineElement();
            frrrl.setValue(gtCodeRuleLineElement);
        }
    }

    private void editDataValue(DataValueRuleLineElement dvrle) {
        ArchetypeElementVO archetypeElementVO = getArchetypeElementVO(dvrle.getParentRuleLine());
        if (archetypeElementVO != null) {
            DVDialogEditor dialog =
                    new DVDialogEditor(
                            gdlEditor.getEditorWindow(),
                            archetypeElementVO,
                            true, true,
                            dvPanelFactory,
                            archetypeManager);
            //Change the behaviour if IS_A (IS_NOT_A) DV_TEXT => allow local term selection
            if (isAnISAComparatorWithDV(dvrle.getParentRuleLine())) {
                dialog.setDVGenericPanel(new DVLocalCodedTextPanel(gdlEditor));
            }
            dialog.getDVGenericPanel().setDataValue(dvrle.getValue());
            dialog.setVisible(true);
            if (dialog.getAnswer()) {
                dvrle.setValue(dialog.getDataValue());
            }
        }
    }

    private void editNullValue(NullValueRuleLineElement nvrle) {
        ArchetypeElementVO archetypeElementVO = getArchetypeElementVO(nvrle.getParentRuleLine());
        if (archetypeElementVO != null) {
            DialogEditorNullValue dialog =
                    new DialogEditorNullValue(gdlEditor.getEditorWindow());
            dialog.setNullValue(nvrle.getValue());
            dialog.setVisible(true);
            if (dialog.getAnswer()) {
                nvrle.setValue(dialog.getSelectedNullValue());
            }
        }
    }

    private static ArchetypeElementVO getArchetypeElementVO(RuleLine ruleLine) {
        ArchetypeElementVO archetypeElementVO = null;
        for (RuleLineElement ruleLineElement : ruleLine.getRuleLineElements()) {
            if (ruleLineElement instanceof ArchetypeElementRuleLineElement) {
                ArchetypeElementRuleLineElement aerle =
                        (ArchetypeElementRuleLineElement) ruleLineElement;
                archetypeElementVO = aerle.getArchetypeElementVO();
                break;
            } else if (ruleLineElement instanceof ArchetypeElementRuleLineDefinitionElement) {
                archetypeElementVO =
                        ((ArchetypeElementRuleLineDefinitionElement) ruleLineElement).getValue();
            } else if (ruleLineElement instanceof PredicateArchetypeElementAttributeRuleLineElement) {
                archetypeElementVO =
                        ((PredicateArchetypeElementAttributeRuleLineElement) ruleLineElement).getValue();
            } else if (ruleLineElement instanceof ArchetypeElementAttributeRuleLineElement) {
                ArchetypeElementRuleLineElement aerle = ((ArchetypeElementAttributeRuleLineElement) ruleLineElement).getValue();
                if (aerle != null) {
                    archetypeElementVO = aerle.getArchetypeElementVO();
                }
            }
        }
        return archetypeElementVO;
    }

    private static boolean isAnISAComparatorWithDV(RuleLine ruleLine) {
        if (ruleLine instanceof ElementComparisonWithDVConditionRuleLine) {
            OperatorKind operatorKind = ((ElementComparisonWithDVConditionRuleLine) ruleLine).getComparisonOperatorRuleLineElement().getValue();
            return operatorKind != null && (operatorKind.equals(OperatorKind.IS_A) || operatorKind.equals(OperatorKind.IS_NOT_A));
        } else if (ruleLine instanceof WithElementPredicateAttributeDefinitionRuleLine) {
            OperatorKind operatorKind = ((WithElementPredicateAttributeDefinitionRuleLine) ruleLine).getComparisonOperatorRuleLineElement().getValue();
            return operatorKind != null && (operatorKind.equals(OperatorKind.IS_A) || operatorKind.equals(OperatorKind.IS_NOT_A));
        } else {
            return false;
        }
    }

    private void editAttribute(ArchetypeElementAttributeRuleLineElement aearle) {
        boolean onlyCDSDomain = (aearle.getParentRuleLine() instanceof ActionRuleLine);
        Object selectedObject = null;
        ArchetypeReference ar = getArchetypeReferenceFromCreateInstanceRuleLine(aearle, onlyCDSDomain);
        DialogElementAttributeFunctionInstanceSelection dialog =
                new DialogElementAttributeFunctionInstanceSelection(
                        gdlEditor,
                        getNodeDefinitionManager(),
                        onlyCDSDomain, ar);
        dialog.setVisible(true);
        if (dialog.getAnswer()) {
            selectedObject = dialog.getSelectedObject();
        }
        if (selectedObject instanceof AttributeFunctionContainerNode) {
            AttributeFunctionContainerNode attributeContainerNode = (AttributeFunctionContainerNode) selectedObject;
            ArchetypeElementRuleLineElement archetypeElementRuleLineElement =
                    new ArchetypeElementRuleLineElement(aearle.getParentRuleLine());
            archetypeElementRuleLineElement.setValue(attributeContainerNode.getGtCodeRuleLineElement());
            aearle.setValue(archetypeElementRuleLineElement);
            aearle.setAttribute(attributeContainerNode.getAttributeFunction());
        } else if (selectedObject instanceof GTCodeRuleLineElement) {
            GTCodeRuleLineElement gtCodeRuleLineElement = (GTCodeRuleLineElement) selectedObject;
            if (gtCodeRuleLineElement.getParentRuleLine() instanceof ArchetypeInstantiationRuleLine) {
                ArchetypeInstantiationRuleLine airl = (ArchetypeInstantiationRuleLine) gtCodeRuleLineElement.getParentRuleLine();
                gdlEditor.addArchetypeElement(airl);
                editAttribute(aearle);
            }
        }
    }

    private void editPredicateAttribute(PredicateArchetypeElementAttributeRuleLineElement paearle) {
        WithElementPredicateExpressionDefinitionRuleLine wepedrl = (WithElementPredicateExpressionDefinitionRuleLine) paearle.getParentRuleLine();
        DialogPredicateElementAttributeInstanceSelection dialog =
                new DialogPredicateElementAttributeInstanceSelection(
                        windowManager,
                        wepedrl.getArchetypeReference().getIdArchetype(),
                        wepedrl.getArchetypeReference().getIdTemplate(),
                        archetypeManager);
        dialog.setVisible(true);
        if (dialog.getAnswer()) {
            Object obj = dialog.getSelectedObject();
            if (obj instanceof PredicateAttributeVO) {
                PredicateAttributeVO predicateAttributeVO = (PredicateAttributeVO) obj;
                paearle.setAttribute(predicateAttributeVO.getAttribute());
                paearle.setValue(predicateAttributeVO.getArchetypeElementVO());
            }
        }
    }

    private static boolean onlyCDSDomain(ArchetypeElementRuleLineElement aearle) {
        return aearle.getParentRuleLine() instanceof ActionRuleLine
                && (!(aearle.getParentRuleLine() instanceof SetElementWithElementActionRuleLine)
                || !aearle.equals(((SetElementWithElementActionRuleLine) aearle.getParentRuleLine()).getSecondArchetypeElementRuleLineElement()));
    }

    private void editExpression(ExpressionRuleLineElement arle) {
        String attribute = getAttributeValue(arle);
        ArchetypeElementVO archetypeElementVO = getArchetypeElementVO(arle.getParentRuleLine());
        if (archetypeElementVO != null && OpenEHRDataValues.UNITS_ATT.equals(attribute)) {
            Collection<String> units = arle.getArchetypeManager().getUnits().getUnits(archetypeElementVO.getIdTemplate(), archetypeElementVO.getId());
            String oldValue = null;
            if (arle.getValue() instanceof StringConstant) {
                oldValue = ((StringConstant) arle.getValue()).getValue();
            }
            DialogComboBoxInsert dialog =
                    new DialogComboBoxInsert(gdlEditor.getEditorWindow(), OpenEHRLanguageManager.getMessage("Units"), oldValue, units);
            dialog.setVisible(true);
            if (dialog.getAnswer()) {
                String unit = dialog.getSelectedItem();
                arle.setValue(new StringConstant(unit));
            }
        } else {
            boolean inPredicate = false;
            if (arle.getParentRuleLine() instanceof WithElementPredicateExpressionDefinitionRuleLine) {
                inPredicate = true;
            }
            ArchetypeReference ar = getArchetypeReferenceFromCreateInstanceRuleLine(arle, true);
            DialogExpressionEditor dialog =
                    new DialogExpressionEditor(windowManager, arle, inPredicate, ar, gdlEditor, getNodeDefinitionManager());
            dialog.setVisible(true);
            if (dialog.getAnswer()) {
                ExpressionItem expressionItem = dialog.getExpressionItem();
                arle.setValue(expressionItem);
            }
        }
    }

    private static String getAttributeValue(ExpressionRuleLineElement arle) {
        for (RuleLineElement rle : arle.getParentRuleLine().getRuleLineElements()) {
            if (rle instanceof ArchetypeElementAttributeRuleLineElement) {
                return ((ArchetypeElementAttributeRuleLineElement) rle).getAttribute();
            }
        }
        return null;
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