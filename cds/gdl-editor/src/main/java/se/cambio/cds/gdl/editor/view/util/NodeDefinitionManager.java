package se.cambio.cds.gdl.editor.view.util;

import se.cambio.cds.controller.session.data.ArchetypeReferencesManager;
import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.readable.rule.ReadableRule;
import se.cambio.cds.gdl.model.readable.rule.RuleLineCollection;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeElementInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeElementRuleLineDefinitionElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeReferenceRuleLineDefinitionElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.GTCodeRuleLineElement;
import se.cambio.cds.gdl.model.readable.util.PredicateAttributeVO;
import se.cambio.cds.gdl.model.readable.util.ReadableArchetypeReferencesUtil;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.util.Domains;
import se.cambio.cds.view.swing.CDSImageUtil;
import se.cambio.cds.view.swing.applicationobjects.DomainsUI;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.controller.session.data.ArchetypeElements;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.OpenEHRDataValuesUI;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.view.trees.SelectableNode;
import se.cambio.openehr.view.trees.SelectableNodeBuilder;
import se.cambio.openehr.view.util.WindowManager;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class NodeDefinitionManager {

    private ArchetypeReferencesManager archetypeReferencesManager;
    private GDLEditor gdlEditor;

    public NodeDefinitionManager(ArchetypeReferencesManager archetypeReferencesManager, GDLEditor gdlEditor) {
        this.archetypeReferencesManager = archetypeReferencesManager;
        this.gdlEditor = gdlEditor;
    }

    public SelectableNode<Object> getElementInstancesSelectionNodes(
            RuleLineCollection definitionRuleLines, boolean onlyCDSDomain, ArchetypeReference ar) {
        SelectableNode<Object> root = new SelectableNodeBuilder<>()
                .setName(GDLEditorLanguageManager.getMessage("Definitions"))
                .setIcon(GDLEditorImageUtil.FOLDER_OBJECT_ICON)
                .createSelectableNode();
        SelectableNode<Object> elementsNode = new SelectableNodeBuilder<>()
                .setName(GDLEditorLanguageManager.getMessage("ElementInstances"))
                .setIcon(GDLEditorImageUtil.FOLDER_OBJECT_ICON)
                .createSelectableNode();
        root.add(elementsNode);
        try {
            addElementInstanceToNode(definitionRuleLines, elementsNode, onlyCDSDomain, ar);
        } catch (InstanceNotFoundException e) {
            ExceptionHandler.handle(e);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        }
        root.add(getArchetypeInstancesSelectionNodes(definitionRuleLines, onlyCDSDomain, ar));
        return root;
    }

    private void addElementInstanceToNode(RuleLineCollection ruleLineCollection, SelectableNode<Object> node, boolean onlyCDSDomain, ArchetypeReference ar)
            throws InstanceNotFoundException, InternalErrorException {
        for (RuleLine ruleLine : ruleLineCollection.getRuleLines()) {
            if (ruleLine instanceof ArchetypeElementInstantiationRuleLine) {
                SelectableNode<Object> nodeAux = getArchetypeElementRuleLineElementNode((ArchetypeElementInstantiationRuleLine) ruleLine, onlyCDSDomain, ar);
                if (nodeAux != null) {
                    node.add(nodeAux);
                }
            }
            addElementInstanceToNode(ruleLine.getChildrenRuleLines(), node, onlyCDSDomain, ar);
        }
    }

    public static SelectableNode<Object> getArchetypeInstancesSelectionNodes(RuleLineCollection definitionRuleLines, boolean onlyCDSDomain, ArchetypeReference ar) {
        SelectableNode<Object> root = new SelectableNodeBuilder<>()
                .setName(GDLEditorLanguageManager.getMessage("ArchetypeInstances"))
                .setIcon(GDLEditorImageUtil.FOLDER_OBJECT_ICON)
                .createSelectableNode();
        try {
            for (RuleLine ruleLine : definitionRuleLines.getRuleLines()) {
                if (ruleLine instanceof ArchetypeInstantiationRuleLine) {
                    SelectableNode<Object> node = getArchetypeInstantiationRuleLineElementNode((ArchetypeInstantiationRuleLine) ruleLine, onlyCDSDomain, ar);
                    if (node != null) {
                        root.add(node);
                    }
                }
            }
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        } catch (InstanceNotFoundException e) {
            ExceptionHandler.handle(e);
        }
        return root;
    }

    private static SelectableNode<Object> getArchetypeInstantiationRuleLineElementNode(ArchetypeInstantiationRuleLine airl, boolean onlyCDSDomain, ArchetypeReference ar)
            throws InternalErrorException, InstanceNotFoundException {
        ArchetypeReference arAux = airl.getArchetypeReference();
        if (arAux != null) {
            String idArchetype = arAux.getIdArchetype();
            if ((!onlyCDSDomain || Domains.CDS_ID.equals(airl.getArchetypeReference().getIdDomain()) && (ar == null || ar.equals(airl.getArchetypeReference())))) {
                return new SelectableNodeBuilder<>()
                        .setName(ReadableArchetypeReferencesUtil.getName(airl))
                        .setDescription(ReadableArchetypeReferencesUtil.getHTMLTooltip(airl))
                        .setIcon(Archetypes.getIcon(idArchetype))
                        .setObject(airl)
                        .createSelectableNode();
            }
        }
        return null;
    }

    private SelectableNode<Object> getArchetypeElementRuleLineElementNode(
            ArchetypeElementInstantiationRuleLine aeirl,
            boolean onlyCDSDomain,
            ArchetypeReference ar) {
        ArchetypeElementVO archetypeElementVO = aeirl.getArchetypeElementRuleLineDefinitionElement().getValue();
        if (archetypeElementVO != null) {
            ArchetypeReference arAux = aeirl.getArchetypeReference();
            if (ar == null || ar.equals(arAux)) {
                String domainId = arAux.getIdDomain();
                if (!onlyCDSDomain || Domains.CDS_ID.equals(domainId)) {
                    String name = gdlEditor.getGTName(aeirl.getGTCodeRuleLineElement().getValue());
                    if (name == null) {
                        name = "*EMPTY*";
                    }
                    name = name.length() > 30 ? name.substring(0, 30) + "..." : name;
                    return new SelectableNodeBuilder<>()
                            .setName(name)
                            .setDescription(archetypeReferencesManager.getHTMLTooltip(archetypeElementVO, arAux))
                            .setIcon(getIcons(aeirl.getGTCodeRuleLineElement()))
                            .setObject(aeirl.getGTCodeRuleLineElement())
                            .createSelectableNode();
                }
            }
        }
        return null;
    }

    private static SelectableNode<Object> getCurrentDateTimeArchetypeElementRuleLineElementNode(GTCodeRuleLineElement currentDateTimeGTCodeRuleLineElement) {
        String name = GDLEditorLanguageManager.getMessage("CurrentDateTime");
        return new SelectableNodeBuilder<>()
                .setName(name)
                .setIcon(OpenEHRDataValuesUI.getIcon(OpenEHRDataValues.DV_DATE_TIME))
                .setObject(currentDateTimeGTCodeRuleLineElement)
                .createSelectableNode();
    }

    private static GTCodeRuleLineElement getCurrentDateTimeGTCodeRuleLineElement() {
        ArchetypeElementInstantiationRuleLine aeirl = new ArchetypeElementInstantiationRuleLine(null);
        GTCodeRuleLineElement currentDateTimeGTCodeRuleLineElement = aeirl.getGTCodeRuleLineElement();
        aeirl.setArchetypeElementVO(ArchetypeElements.CURRENT_DATE_TIME);
        currentDateTimeGTCodeRuleLineElement.setValue(OpenEHRConst.CURRENT_DATE_TIME_ID);
        return currentDateTimeGTCodeRuleLineElement;
    }

    private ImageIcon getIcons(GTCodeRuleLineElement gtCodeRuleLineElement) {
        RuleLine ruleLine = gtCodeRuleLineElement.getParentRuleLine();
        if (ruleLine instanceof ArchetypeElementInstantiationRuleLine) {
            ArchetypeElementRuleLineDefinitionElement aerlde =
                    ((ArchetypeElementInstantiationRuleLine) ruleLine).getArchetypeElementRuleLineDefinitionElement();
            if (aerlde.getValue() != null) {
                return getIconsArchetypeElement(aerlde);
            }
        } else if (ruleLine instanceof ArchetypeInstantiationRuleLine) {
            ArchetypeReferenceRuleLineDefinitionElement arrlde =
                    ((ArchetypeInstantiationRuleLine) ruleLine).getArchetypeReferenceRuleLineDefinitionElement();
            if (arrlde.getValue() != null) {
                return getIconsArchetypeReference(arrlde);
            }
        }
        return null;
    }

    private static ImageIcon getIconsArchetypeElement(ArchetypeElementRuleLineDefinitionElement aerlde) {
        String archetypeId = aerlde.getValue().getIdArchetype();
        String archReferenceRM = Archetypes.getEntryType(archetypeId);
        String archElementRM = aerlde.getValue().getRMType();
        return new MultipleIcon(
                new Icon[]{
                        DomainsUI.getGroupIconFromArchetypeReference(aerlde.getArchetypeReference()),
                        OpenEHRConstUI.getIcon(archReferenceRM),
                        OpenEHRDataValuesUI.getIcon(archElementRM)});
    }


    private static ImageIcon getIconsArchetypeReference(ArchetypeReferenceRuleLineDefinitionElement arrlde) {
        String archetypeId = arrlde.getValue().getIdArchetype();
        String archReferenceRM = Archetypes.getEntryType(archetypeId);
        return new MultipleIcon(
                new Icon[]{
                        DomainsUI.getGroupIconFromArchetypeReference(arrlde.getValue()),
                        OpenEHRConstUI.getIcon(archReferenceRM)});
    }

    public static SelectableNode<Object> getElementsInArchetypeNode(
            String idArchetype, String idTemplate,
            ArchetypeManager archetypeManager) throws InternalErrorException, InstanceNotFoundException {
        ArchetypeDTO archetypeVO = archetypeManager.getArchetypes().getCMElement(idArchetype);
        SelectableNode.SelectionMode selectionMode = getSelectionMode();
        SelectableNode<Object> rootNode = new SelectableNodeBuilder<>()
                .setName(idArchetype)
                .setDescription(idArchetype)
                .setSelectionMode(selectionMode)
                .setIcon(Archetypes.getIcon(archetypeVO.getId()))
                .createSelectableNode();
        Map<String, SelectableNode<Object>> rmNodes =
                new HashMap<>();

        Map<Object, SelectableNode<Object>> clusters;
        clusters = new HashMap<>();

        SelectableNode<Object> nodoOrigen;
        Collection<ArchetypeElementVO> archetypeElementVOs = archetypeManager.getArchetypeElements().getArchetypeElementsVO(idArchetype, idTemplate);
        for (ArchetypeElementVO archetypeElementVO : archetypeElementVOs) {
            SelectableNode<Object> rmNode = ClusterNodesUtil.getRMNode(rootNode, rmNodes, archetypeElementVO.getPath());
            SelectableNode<Object> clusterNode =
                    ClusterNodesUtil.getClusterNode(
                            idTemplate,
                            archetypeElementVO.getParentId(),
                            rmNode, clusters,
                            true,
                            true,
                            archetypeManager);
            nodoOrigen = createElementNode(archetypeElementVO, selectionMode, archetypeManager);
            clusterNode.add(nodoOrigen);
        }
        return rootNode;
    }

    private static SelectableNode.SelectionMode getSelectionMode() {
        return SelectableNode.SelectionMode.SINGLE;
    }

    private static SelectableNode<Object> createElementNode(ArchetypeElementVO archetypeElementVO, SelectableNode.SelectionMode selectionMode, ArchetypeManager archetypeManager) {
        String name = archetypeManager.getArchetypeElements().getText(archetypeElementVO, UserConfigurationManager.instance().getLanguage());
        String desc = archetypeManager.getArchetypeElements().getDescription(archetypeElementVO, UserConfigurationManager.instance().getLanguage());
        return new SelectableNodeBuilder<>()
                .setName(name)
                .setDescription(desc)
                .setSelectionMode(selectionMode)
                .setIcon(OpenEHRDataValuesUI.getIcon(archetypeElementVO.getRMType()))
                .setObject(archetypeElementVO)
                .createSelectableNode();
    }

    public void addElementInstanceAttributesAndFunctionsToNode(
            RuleLineCollection ruleLineCollection,
            SelectableNode<Object> node,
            boolean onlyCDSDomain,
            ArchetypeReference ar) {
        for (RuleLine ruleLine : ruleLineCollection.getRuleLines()) {
            if (ruleLine instanceof ArchetypeElementInstantiationRuleLine) {
                ArchetypeElementInstantiationRuleLine aeirl = (ArchetypeElementInstantiationRuleLine) ruleLine;
                if (aeirl.getArchetypeElement() != null) {
                    SelectableNode<Object> nodeAux =
                            getArchetypeElementRuleLineElementNode(aeirl, onlyCDSDomain, ar);
                    if (nodeAux != null) {
                        GTCodeRuleLineElement gtCodeRuleLineElement =
                                (GTCodeRuleLineElement) nodeAux.getObject();
                        addFieldsToNode(nodeAux, aeirl.getArchetypeElement().getRMType(), gtCodeRuleLineElement);
                        addFunctionsToNode(nodeAux, gtCodeRuleLineElement);
                        node.add(nodeAux);
                    }
                }
            }
            addElementInstanceAttributesAndFunctionsToNode(ruleLine.getChildrenRuleLines(), node, onlyCDSDomain, ar);
        }
    }

    private static void addFieldsToNode(SelectableNode<Object> node, String rmName, GTCodeRuleLineElement gtCodeRuleLineElement) {
        node.setObject(null);
        String[] fieldNames =
                OpenEHRDataValuesUI.getFieldNames(rmName);
        for (String fieldName : fieldNames) {
            AttributeFunctionContainerNode attributeNode = new AttributeFunctionContainerNode(
                    gtCodeRuleLineElement,
                    fieldName);
            SelectableNode<Object> fieldNode =
                    new SelectableNodeBuilder<>()
                            .setName(fieldName)
                            .setIcon(GDLEditorImageUtil.OBJECT_ICON)
                            .setObject(attributeNode)
                            .createSelectableNode();
            node.add(fieldNode);
        }
    }

    private static void addFunctionsToNode(SelectableNode<?> node, GTCodeRuleLineElement gtCodeRuleLineElement) {
        node.setObject(null);
        ArrayList<String> functionNames =
                OpenEHRDataValuesUI.getFunctionNames();
        for (String functionName : functionNames) {
            AttributeFunctionContainerNode functionNode = new AttributeFunctionContainerNode(
                    gtCodeRuleLineElement,
                    functionName);
            SelectableNode<Object> fieldNode =
                    new SelectableNodeBuilder<>()
                            .setName(functionName)
                            .setIcon(GDLEditorImageUtil.FUNCTION_ICON)
                            .setObject(functionNode)
                            .createSelectableNode();
            node.add(fieldNode);
        }
    }

    public static SelectableNode<Object> getSingleNodeAttributesAndFunctions() {
        String name = GDLEditorLanguageManager.getMessage("Attributes") + "/" + GDLEditorLanguageManager.getMessage("Functions");
        return new SelectableNodeBuilder<>()
                .setName(name)
                .setIcon(GDLEditorImageUtil.OBJECT_ICON)
                .createSelectableNode();
    }

    public SelectableNode<Object> getNodeAttributesAndFunctions(GDLEditor gdlEditor, boolean onlyCDSDomain, ArchetypeReference ar) {
        RuleLineCollection definitionRuleLines = gdlEditor.getDefinitionRuleLines();
        SelectableNode<Object> root = getSingleNodeAttributesAndFunctions();
        SelectableNode<Object> elementsNode = getElementsNode();
        root.add(elementsNode);
        addElementInstanceAttributesAndFunctionsToNode(definitionRuleLines, elementsNode, onlyCDSDomain, ar);
        if (!onlyCDSDomain) {
            SelectableNode<Object> currentDateTimeNode = getCurrentTimeNodeWithAttributes();
            elementsNode.add(currentDateTimeNode);
        }
        SelectableNode<GTCodeRuleLineElement> rulesNode = getGTCodeRuleLineElementNodes(gdlEditor.getRenderableRules(), true);
        root.add(rulesNode);
        root.add(getArchetypeInstancesSelectionNodes(definitionRuleLines, onlyCDSDomain, ar));
        return root;
    }

    private static SelectableNode<Object> getCurrentTimeNodeWithAttributes() {
        GTCodeRuleLineElement currentDateTimeGTCodeRuleLineElement = getCurrentDateTimeGTCodeRuleLineElement();
        SelectableNode<Object> currentDateTimeNode =
                getCurrentDateTimeArchetypeElementRuleLineElementNode(currentDateTimeGTCodeRuleLineElement);
        addFieldsToNode(currentDateTimeNode, OpenEHRDataValues.DV_DATE_TIME, getCurrentDateTimeGTCodeRuleLineElement());
        return currentDateTimeNode;
    }

    public static SelectableNode<Object> getNodeAttributesAndFunctionsPredicate() {
        SelectableNode<Object> root = new SelectableNodeBuilder<>()
                .setName(GDLEditorLanguageManager.getMessage("Attributes"))
                .setIcon(GDLEditorImageUtil.OBJECT_ICON)
                .createSelectableNode();
        SelectableNode<Object> currentDateTimeNode = getCurrentTimeNodeWithAttributes();
        root.add(currentDateTimeNode);
        return root;
    }

    public static SelectableNode<Object> getNodeAttributesAndFunctions(String archetypteId, String templateId, ArchetypeManager archetypeManager) {
        SelectableNode<Object> root =
                new SelectableNodeBuilder<>()
                        .setName(GDLEditorLanguageManager.getMessage("Attributes"))
                        .setIcon(GDLEditorImageUtil.OBJECT_ICON)
                        .createSelectableNode();
        Collection<ArchetypeElementVO> archetypeElementVOs = archetypeManager.getArchetypeElements().getArchetypeElementsVO(archetypteId, templateId);
        for (ArchetypeElementVO archetypeElementVO : archetypeElementVOs) {
            SelectableNode<Object> elementNode = createElementNode(archetypeElementVO, SelectableNode.SelectionMode.SINGLE, archetypeManager);
            String[] fieldNames =
                    OpenEHRDataValuesUI.getFieldNames(archetypeElementVO.getRMType());
            for (String fieldName : fieldNames) {
                PredicateAttributeVO predicateAttributeVO = new PredicateAttributeVO(archetypeElementVO, fieldName);
                SelectableNode<Object> fieldNode =
                        new SelectableNodeBuilder<>()
                                .setName(fieldName)
                                .setIcon(GDLEditorImageUtil.OBJECT_ICON)
                                .setObject(predicateAttributeVO)
                                .createSelectableNode();
                elementNode.add(fieldNode);
            }
            root.add(elementNode);
        }
        return root;
    }

    public static SelectableNode<Object> getNodeGTCodes(Map<String, Term> termsMap, Collection<String> gtCodesToBeIgnored) {
        SelectableNode<Object> root =
                new SelectableNodeBuilder<>()
                        .setName(GDLEditorLanguageManager.getMessage("LocalTerms"))
                        .setIcon(GDLEditorImageUtil.OBJECT_ICON)
                        .createSelectableNode();
        ArrayList<String> terms = new ArrayList<>(termsMap.keySet());
        Collections.sort(terms);
        for (String gtCode : terms) {
            if (!gtCodesToBeIgnored.contains(gtCode)) {
                String name = termsMap.get(gtCode).getText();
                String gtDesc = gtCode + (name != null ? " - " + name : "");
                SelectableNode<Object> nodeAux =
                        new SelectableNodeBuilder<>()
                                .setName(gtDesc)
                                .setIcon(GDLEditorImageUtil.OBJECT_ICON)
                                .setObject(gtCode)
                                .createSelectableNode();
                root.add(nodeAux);
            }
        }
        return root;
    }

    public static SelectableNode<Object> getNodeTerminologyIds(List<String> terminologyIds) {
        SelectableNode<Object> root =
                new SelectableNodeBuilder<>()
                        .setName(GDLEditorLanguageManager.getMessage("Terminologies"))
                        .setIcon(OpenEHRImageUtil.TERMSET)
                        .createSelectableNode();
        for (String terminologyId : terminologyIds) {
            SelectableNode<Object> nodeAux =
                    new SelectableNodeBuilder<>()
                            .setName(terminologyId)
                            .setIcon(OpenEHRImageUtil.TERMSET)
                            .setObject(terminologyId)
                            .createSelectableNode();
            root.add(nodeAux);
        }
        return root;
    }

    public static SelectableNode<GTCodeRuleLineElement> getGTCodeRuleLineElementNodes(LinkedHashMap<String, ReadableRule> renderableRules, boolean showAttributes) {
        SelectableNode<GTCodeRuleLineElement> rulesNode = getRulesNode();
        addRuleNodes(renderableRules, rulesNode, showAttributes);
        return rulesNode;
    }

    private static SelectableNode<Object> getElementsNode() {
        return new SelectableNodeBuilder<>()
                .setName(GDLEditorLanguageManager.getMessage("Elements"))
                .setIcon(GDLEditorImageUtil.FOLDER_OBJECT_ICON)
                .createSelectableNode();
    }

    private static void addRuleNodes(LinkedHashMap<String, ReadableRule> renderableRules, SelectableNode<GTCodeRuleLineElement> rootNode, boolean showAttributes) {
        for (ReadableRule readableRule : renderableRules.values()) {
            GTCodeRuleLineElement gtCodeRuleLineElement = readableRule.getDefinitionRuleLine().getGTCodeRuleLineElement();
            String ruleName = readableRule.getTermDefinition().getTermText(readableRule.getGTCode());
            GTCodeRuleLineElement ruleObject = showAttributes ? null : gtCodeRuleLineElement;
            SelectableNode<GTCodeRuleLineElement> ruleNode = getRulesNode();
            ruleNode.setName(ruleName);
            ruleNode.setObject(ruleObject);
            rootNode.add(ruleNode);
            if (showAttributes) {
                addFunctionsToNode(ruleNode, gtCodeRuleLineElement);
            }
        }
    }

    private static SelectableNode<GTCodeRuleLineElement> getRulesNode() {
        return new SelectableNodeBuilder<GTCodeRuleLineElement>()
                .setName(GDLEditorLanguageManager.getMessage("Rules"))
                .setIcon(CDSImageUtil.RULE_ICON)
                .setObject(null)
                .createSelectableNode();
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