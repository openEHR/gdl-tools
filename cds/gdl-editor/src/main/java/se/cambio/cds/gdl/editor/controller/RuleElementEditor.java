package se.cambio.cds.gdl.editor.controller;

import se.cambio.cds.gdl.editor.util.DefinitionDependencyChecker;
import se.cambio.cds.gdl.editor.view.dialog.*;
import se.cambio.cds.gdl.editor.view.panels.DVLocalCodedTextPanel;
import se.cambio.cds.gdl.editor.view.util.AttributeFunctionContainerNode;
import se.cambio.cds.gdl.editor.view.util.NodeDefinitionConversor;
import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.expression.ExpressionItem;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.gdl.model.expression.StringConstant;
import se.cambio.cds.gdl.model.readable.rule.lines.*;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.*;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ActionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ArchetypeReferenceRuleLine;
import se.cambio.cds.gdl.model.readable.util.PredicateAttributeVO;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.util.Domains;
import se.cambio.cds.view.swing.dialogs.DialogArchetypeChooser;
import se.cambio.openehr.controller.session.data.Units;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.view.dialogs.DVDialogEditor;
import se.cambio.openehr.view.dialogs.DialogEditorNullValue;
import se.cambio.openehr.view.trees.SelectableNode;

import java.awt.*;
import java.util.Collection;
import java.util.Iterator;

public class RuleElementEditor {

    public static void edit(RuleLineElementWithValue<?> ruleLineElementWithValue){
        if (ruleLineElementWithValue instanceof ArchetypeReferenceRuleLineDefinitionElement){
            editArchetype((ArchetypeReferenceRuleLineDefinitionElement)ruleLineElementWithValue);
        }else if (ruleLineElementWithValue instanceof GTCodeRuleLineElement){
            rename((GTCodeRuleLineElement)ruleLineElementWithValue);
        }else if (ruleLineElementWithValue instanceof ArchetypeElementRuleLineDefinitionElement){
            editArchetypeElement((ArchetypeElementRuleLineDefinitionElement)ruleLineElementWithValue);
        }else if (ruleLineElementWithValue instanceof ArchetypeElementRuleLineElement){
            selectArchetypeElement((ArchetypeElementRuleLineElement)ruleLineElementWithValue);
        }else if (ruleLineElementWithValue instanceof DataValueRuleLineElement){
            editDataValue((DataValueRuleLineElement)ruleLineElementWithValue);
        }else if (ruleLineElementWithValue instanceof NullValueRuleLineElement){
            editNullValue((NullValueRuleLineElement)ruleLineElementWithValue);
        }else if (ruleLineElementWithValue instanceof PredicateArchetypeElementAttributeRuleLineElement){
            editPredicateAttribute((PredicateArchetypeElementAttributeRuleLineElement) ruleLineElementWithValue);
        }else if (ruleLineElementWithValue instanceof ArchetypeElementAttributeRuleLineElement){
            editAttribute((ArchetypeElementAttributeRuleLineElement) ruleLineElementWithValue);
        }else if (ruleLineElementWithValue instanceof ExpressionRuleLineElement){
            editExpression((ExpressionRuleLineElement)ruleLineElementWithValue);
        }
    }

    public static void editArchetype(ArchetypeReferenceRuleLineDefinitionElement arrlde){
        ArchetypeInstantiationRuleLine airl = (ArchetypeInstantiationRuleLine)arrlde.getParentRuleLine();
        boolean showOnlyCDS = DefinitionDependencyChecker.isBeingUsedInAction(airl, EditorManager.getActiveGDLEditor());
        editArchetype(arrlde, showOnlyCDS);
    }

    public static void editArchetype(ArchetypeReferenceRuleLineDefinitionElement arrlde, boolean showOnlyCDS){
        Window owner = EditorManager.getActiveEditorWindow();
        ArchetypeReference ar = arrlde.getValue();
        String idArchetype = ar!=null?ar.getIdArchetype():null;
        String idTemplate = ar!=null?ar.getIdTemplate():null;
        boolean isTemplate = idTemplate!=null;
        String domainId = ar!=null?ar.getIdDomain():null;
        DialogArchetypeChooser dialog = new DialogArchetypeChooser(owner, idArchetype, domainId, isTemplate, showOnlyCDS);
        dialog.setVisible(true);
        if (dialog.getAnswer()){
            idArchetype = dialog.getSelectedArchetypeId();
            if (idArchetype==null){
                if (ar!=null){
                    idArchetype = ar.getIdArchetype();
                    idTemplate = ar.getIdTemplate();
                }
            }else{
                idTemplate = dialog.getSelectedTemplateId();
            }
            if (idArchetype!=null){
                String idDomain = dialog.getSelectedDomain();
                ar = new ArchetypeReference(idDomain, idArchetype, idTemplate);
                arrlde.setValue(ar);
                //RuleLine ruleLine = arrlde.getParentRuleLine();
            }
        }
    }

    public static void rename(GTCodeRuleLineElement gtCodeRuleLineElement){
        Window owner = EditorManager.getActiveEditorWindow();
        String title = null;
        RuleLine ruleLine = gtCodeRuleLineElement.getParentRuleLine();
        if (ruleLine instanceof ArchetypeInstantiationRuleLine){
            title = ((ArchetypeInstantiationRuleLine)ruleLine).getArchetypeReferenceRuleLineDefinitionElement().getText();
        }else if (ruleLine instanceof ArchetypeElementInstantiationRuleLine){
            title = ((ArchetypeElementInstantiationRuleLine)ruleLine).getArchetypeElementRuleLineDefinitionElement().getText();
        }
        Term term = EditorManager.getActiveGDLEditor().getTerm(gtCodeRuleLineElement.getValue());
        String oldValue = term.getText();
        DialogNameInsert dialog = new DialogNameInsert(owner, title, oldValue);
        if (dialog.getAnswer()){
            term.setText(dialog.getValue());
        }
    }

    public static void editArchetypeElement(ArchetypeElementRuleLineDefinitionElement aerlde){
        ArchetypeReference ar = aerlde.getArchetypeReference();
        if (ar!=null){
            SelectableNode<Object> node =
                    NodeDefinitionConversor.getElementsInArchetypeNode(ar.getIdArchetype(), ar.getIdTemplate(), true, true);
            DialogElementSelection dialog = new DialogElementSelection(EditorManager.getActiveEditorWindow(), node);
            dialog.setVisible(true);
            if (dialog.getAnswer()){
                Object obj = dialog.getSelectedObject();
                if (obj instanceof ArchetypeElementVO){
                    aerlde.setValue((ArchetypeElementVO)obj);
                    RuleLine ruleLine = aerlde.getParentRuleLine();
                    if (ruleLine instanceof ArchetypeElementInstantiationRuleLine){
                        ArchetypeElementInstantiationRuleLine aeirl =
                                (ArchetypeElementInstantiationRuleLine)ruleLine;
                        String gtCode = aeirl.getGTCodeRuleLineElement().getValue();
                        Term term = EditorManager.getActiveGDLEditor().getTerm(gtCode);
                        if (term.getText()==null || term.getText().isEmpty()){
                            term.setText(aerlde.getValue().getName());
                            term.setDescription(aerlde.getValue().getDescription());
                        }
                    }
                }
            }
        }
    }

    public static void selectArchetypeElement(ArchetypeElementRuleLineElement arrle){
        GDLEditor controller = EditorManager.getActiveGDLEditor();
        boolean onlyCDSDomain = onlyCDSDomain(arrle);
        Object selectedObject = null;
        //if (!emptyDefinitions(controller.getDefinitionRuleLines(), onlyCDSDomain)){
        DialogElementInstanceSelection dialog =
                new DialogElementInstanceSelection(EditorManager.getActiveEditorWindow(), controller, onlyCDSDomain);
        dialog.setVisible(true);
        if (dialog.getAnswer()){
            selectedObject = dialog.getSelectedObject();
        }
        //}else{//First time
	/*
	    ArchetypeInstantiationRuleLine airl = 
		    controller.addArchetypeReference(onlyCDSDomain);
	    if (airl!=null){
		ArchetypeElementInstantiationRuleLine aeirl =
			controller.addArchetypeElement(airl);
		if (aeirl!=null){
		    selectedObject = aeirl.getGTCodeRuleLineElement();
		}
	    }
	}*/
        if (selectedObject instanceof GTCodeRuleLineElement){
            GTCodeRuleLineElement gtCodeRuleLineElement = (GTCodeRuleLineElement)selectedObject;
            if (gtCodeRuleLineElement.getParentRuleLine() instanceof ArchetypeElementInstantiationRuleLine){
                arrle.setValue(gtCodeRuleLineElement);
            }
        }else if (selectedObject instanceof ArchetypeInstantiationRuleLine){
            ArchetypeInstantiationRuleLine airl = (ArchetypeInstantiationRuleLine)selectedObject;
            ArchetypeElementInstantiationRuleLine aeirl = controller.addArchetypeElement(airl);
            if (aeirl!=null){
                arrle.setValue(aeirl.getGTCodeRuleLineElement());
            }
        }
    }

    public static boolean emptyDefinitions(Collection<RuleLine> definitionRuleLines, boolean onlyShowCDS){
        if (!onlyShowCDS){
            return definitionRuleLines.isEmpty();
        }else{
            Iterator<RuleLine> i = definitionRuleLines.iterator();
            while(i.hasNext()){
                RuleLine ruleLine = i.next();
                if (ruleLine instanceof ArchetypeReferenceRuleLine){
                    ArchetypeReference ar = ((ArchetypeReferenceRuleLine)ruleLine).getArchetypeReference();
                    if (Domains.CDS_ID.equals(ar.getIdDomain())){
                        return false;
                    }
                }
            }
            return true;
        }
    }

    public static void editDataValue(DataValueRuleLineElement dvrle){
        ArchetypeElementVO archetypeElementVO = getArchetypeElementVO(dvrle.getParentRuleLine());
        if (archetypeElementVO!=null){
            DVDialogEditor dialog =
                    new DVDialogEditor(
                            EditorManager.getActiveEditorWindow(),
                            archetypeElementVO,
                            true, true);
            //Change the behaviour if IS_A (IS_NOT_A) DV_TEXT => allow local term selection
            if (isAnISAComparatorWithDV(dvrle.getParentRuleLine())){
                dialog.setDVGenericPanel(new DVLocalCodedTextPanel(EditorManager.getActiveGDLEditor()));
            }
            dialog.getDVGenericPanel().setDataValue(dvrle.getValue());
            dialog.setVisible(true);
            if(dialog.getAnswer()){
                dvrle.setValue(dialog.getDataValue());
            }
        }
    }

    public static void editNullValue(NullValueRuleLineElement nvrle){
        ArchetypeElementVO archetypeElementVO = getArchetypeElementVO(nvrle.getParentRuleLine());
        if (archetypeElementVO!=null){
            DialogEditorNullValue dialog =
                    new DialogEditorNullValue(EditorManager.getActiveEditorWindow());
            dialog.setNullValue(nvrle.getValue());
            dialog.setVisible(true);
            if(dialog.getAnswer()){
                nvrle.setValue(dialog.getSelectedNullValue());
            }
        }
    }

    private static ArchetypeElementVO getArchetypeElementVO(RuleLine ruleLine){
        ArchetypeElementVO archetypeElementVO = null;
        for (RuleLineElement ruleLineElement : ruleLine.getRuleLineElements()) {
            if (ruleLineElement instanceof ArchetypeElementRuleLineElement){
                ArchetypeElementRuleLineElement aerle =
                        (ArchetypeElementRuleLineElement) ruleLineElement;
                archetypeElementVO = aerle.getArchetypeElementVO();
                break;
            }else if (ruleLineElement instanceof ArchetypeElementRuleLineDefinitionElement){
                archetypeElementVO =
                        ((ArchetypeElementRuleLineDefinitionElement)ruleLineElement).getValue();
            }else if (ruleLineElement instanceof PredicateArchetypeElementAttributeRuleLineElement){
                archetypeElementVO =
                        ((PredicateArchetypeElementAttributeRuleLineElement)ruleLineElement).getValue();
            }else if (ruleLineElement instanceof ArchetypeElementAttributeRuleLineElement){
                ArchetypeElementRuleLineElement aerle = ((ArchetypeElementAttributeRuleLineElement)ruleLineElement).getValue();
                if (aerle!=null){
                    archetypeElementVO = aerle.getArchetypeElementVO();
                }
            }
        }
        return archetypeElementVO;
    }

    private static boolean isAnISAComparatorWithDV(RuleLine ruleLine){
        if (ruleLine instanceof ElementComparisonWithDVConditionRuleLine){
            OperatorKind operatorKind = ((ElementComparisonWithDVConditionRuleLine)ruleLine).getComparisonOperatorRuleLineElement().getValue();
            return operatorKind!=null && (operatorKind.equals(OperatorKind.IS_A) || operatorKind.equals(OperatorKind.IS_NOT_A));
        }else if (ruleLine instanceof WithElementPredicateAttributeDefinitionRuleLine){
            OperatorKind operatorKind = ((WithElementPredicateAttributeDefinitionRuleLine)ruleLine).getComparisonOperatorRuleLineElement().getValue();
            return operatorKind!=null && (operatorKind.equals(OperatorKind.IS_A) || operatorKind.equals(OperatorKind.IS_NOT_A));
        }else{
            return false;
        }
    }

    public static void editAttribute(ArchetypeElementAttributeRuleLineElement aearle){
        GDLEditor controller = EditorManager.getActiveGDLEditor();
        boolean onlyCDSDomain = (aearle.getParentRuleLine() instanceof ActionRuleLine);
        Object selectedObject = null;
        //if (!emptyDefinitions(controller.getDefinitionRuleLines(), onlyCDSDomain)){
        DialogElementAttributeFunctionInstanceSelection dialog =
                new DialogElementAttributeFunctionInstanceSelection(EditorManager.getActiveEditorWindow(), controller, onlyCDSDomain);
        dialog.setVisible(true);
        if (dialog.getAnswer()){
            selectedObject = dialog.getSelectedObject();
        }
        //}else{//First element
	    /*
	    ArchetypeInstantiationRuleLine airl = 
		    controller.addArchetypeReference(onlyCDSDomain);
	    if (airl!=null){
		ArchetypeElementInstantiationRuleLine aeirl =
			controller.addArchetypeElement(airl);
		if (aeirl!=null){
		    DialogElementAttributeFunctionInstanceSelection dialog =
			    new DialogElementAttributeFunctionInstanceSelection(EditorManager.getActiveEditorWindow(), controller, onlyCDSDomain);
		    dialog.setVisible(true);
		    if (dialog.getAnswer()){
			selectedObject = dialog.getSelectedObject();
		    }
		}
	    }
	}*/
        if (selectedObject instanceof AttributeFunctionContainerNode){
            AttributeFunctionContainerNode attributeContainerNode = (AttributeFunctionContainerNode) selectedObject;
            ArchetypeElementRuleLineElement archetypeElementRuleLineElement =
                    new ArchetypeElementRuleLineElement(aearle.getParentRuleLine());
            archetypeElementRuleLineElement.setValue(attributeContainerNode.getGtCodeRuleLineElement());
            aearle.setValue(archetypeElementRuleLineElement);
            aearle.setAttribute(attributeContainerNode.getAttributeFunction());
        }else if (selectedObject instanceof GTCodeRuleLineElement){
            GTCodeRuleLineElement gtCodeRuleLineElement = (GTCodeRuleLineElement)selectedObject;
            if (gtCodeRuleLineElement.getParentRuleLine() instanceof ArchetypeInstantiationRuleLine){
                ArchetypeInstantiationRuleLine airl = (ArchetypeInstantiationRuleLine)gtCodeRuleLineElement.getParentRuleLine();
                controller.addArchetypeElement(airl);
                editAttribute(aearle);
            }
        }
    }

    private static void editPredicateAttribute(PredicateArchetypeElementAttributeRuleLineElement paearle){
        GDLEditor controller = EditorManager.getActiveGDLEditor();
        WithElementPredicateExpressionDefinitionRuleLine wepedrl = (WithElementPredicateExpressionDefinitionRuleLine)paearle.getParentRuleLine();
        DialogPredicateElementAttributeInstanceSelection dialog =
                new DialogPredicateElementAttributeInstanceSelection(
                        EditorManager.getActiveEditorWindow(),
                        controller,
                        wepedrl.getArchetypeReference().getIdArchetype(),
                        wepedrl.getArchetypeReference().getIdTemplate());
        dialog.setVisible(true);
        if (dialog.getAnswer()){
            Object obj = dialog.getSelectedObject();
            if (obj instanceof PredicateAttributeVO){
                PredicateAttributeVO predicateAttributeVO = (PredicateAttributeVO)obj;
                paearle.setAttribute(predicateAttributeVO.getAttribute());
                paearle.setValue(predicateAttributeVO.getArchetypeElementVO());
            }
        }
    }

    private static boolean onlyCDSDomain(ArchetypeElementRuleLineElement aearle){
        if (aearle.getParentRuleLine() instanceof ActionRuleLine){
            if (aearle.getParentRuleLine() instanceof SetElementWithElementActionRuleLine){
                return !aearle.equals(((SetElementWithElementActionRuleLine)aearle.getParentRuleLine()).getSecondArchetypeElementRuleLineElement());
            }else{
                return true;
            }
        }else{
            return false;
        }
    }

    public static void editExpression(ExpressionRuleLineElement arle){
        ArchetypeElementVO archetypeElementVO = getArchetypeElementVO(arle.getParentRuleLine());
        if (archetypeElementVO!=null){
            String attribute = getAttributeValue(arle);
            if (OpenEHRDataValues.UNITS_ATT.equals(attribute)){
                Collection<String> units = Units.getUnits(archetypeElementVO.getIdTemplate(), archetypeElementVO.getId());
                String oldValue = null;
                if (arle.getValue() instanceof StringConstant){
                    oldValue = ((StringConstant)arle.getValue()).getValue();
                }
                DialogComboBoxInsert dialog =
                        new DialogComboBoxInsert(EditorManager.getActiveEditorWindow(), OpenEHRLanguageManager.getMessage("Units"), oldValue, units);
                dialog.setVisible(true);
                if (dialog.getAnswer()){
                    String unit = dialog.getSelectedItem();
                    arle.setValue(new StringConstant(unit));
                }
            }else{
                boolean inPredicate = false;
                if (arle.getParentRuleLine() instanceof WithElementPredicateExpressionDefinitionRuleLine){
                    inPredicate = true;
                }
                DialogExpressionEditor dialog =
                        new DialogExpressionEditor(EditorManager.getActiveEditorWindow(), archetypeElementVO, arle, inPredicate);
                dialog.setVisible(true);
                if (dialog.getAnswer()){
                    ExpressionItem expressionItem = dialog.getExpressionItem();
                    arle.setValue(expressionItem);
                }
            }
        }
    }

    private static String getAttributeValue(ExpressionRuleLineElement arle){
        Iterator<RuleLineElement> i = arle.getParentRuleLine().getRuleLineElements().iterator();
        while (i.hasNext()){
            RuleLineElement rle = i.next();
            if (rle instanceof ArchetypeElementAttributeRuleLineElement){
                return ((ArchetypeElementAttributeRuleLineElement)rle).getAttribute();
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