package se.cambio.cds.gdl.editor.view.util;

import se.cambio.cds.controller.session.data.ArchetypeReferences;
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

import javax.swing.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class NodeDefinitionConversor {

    public static SelectableNode<Object> getElementInstancesSelectionNodes(
            RuleLineCollection definitionRuleLines, boolean onlyCDSDomain, ArchetypeReference ar) {
        SelectableNode<Object> root = new SelectableNodeBuilder<Object>()
                .setName(GDLEditorLanguageManager.getMessage("Definitions"))
                .setIcon(GDLEditorImageUtil.FOLDER_OBJECT_ICON)
                .createSelectableNode();
        SelectableNode<Object> elementsNode = new SelectableNodeBuilder<Object>()
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

    public static void addElementInstanceToNode(RuleLineCollection ruleLineCollection, SelectableNode<Object> node, boolean onlyCDSDomain, ArchetypeReference ar)
            throws InstanceNotFoundException, InternalErrorException {
        for (RuleLine ruleLine : ruleLineCollection.getRuleLines()) {
            if (ruleLine instanceof ArchetypeElementInstantiationRuleLine){
                SelectableNode<Object> nodeAux = getArchetypeElementRuleLineElementNode((ArchetypeElementInstantiationRuleLine)ruleLine, onlyCDSDomain, ar);
                if(nodeAux != null){
                    node.add(nodeAux);
                }
            }
            addElementInstanceToNode(ruleLine.getChildrenRuleLines(), node, onlyCDSDomain, ar);
        }
    }

    public static SelectableNode<Object> getArchetypeInstancesSelectionNodes(RuleLineCollection definitionRuleLines, boolean onlyCDSDomain, ArchetypeReference ar){
        SelectableNode<Object> root =  new SelectableNodeBuilder<Object>()
                .setName(GDLEditorLanguageManager.getMessage("ArchetypeInstances"))
                .setIcon(GDLEditorImageUtil.FOLDER_OBJECT_ICON)
                .createSelectableNode();
        try {
            for (RuleLine ruleLine : definitionRuleLines.getRuleLines()) {
                if (ruleLine instanceof ArchetypeInstantiationRuleLine){
                    SelectableNode<Object> node = getArchetypeElementRuleLineElementNode((ArchetypeInstantiationRuleLine)ruleLine, onlyCDSDomain, ar);
                    if (node!=null){
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

    public static SelectableNode<Object> getArchetypeElementRuleLineElementNode(ArchetypeInstantiationRuleLine airl, boolean onlyCDSDomain, ArchetypeReference ar)
            throws InternalErrorException, InstanceNotFoundException {
        ArchetypeReference arAux = airl.getArchetypeReference();
        if (arAux != null){
            String idArchetype = arAux.getIdArchetype();
            if ((!onlyCDSDomain || Domains.CDS_ID.equals(airl.getArchetypeReference().getIdDomain()) && (ar == null || ar.equals(airl.getArchetypeReference())))){
                return new SelectableNodeBuilder<Object>()
                        .setName(ReadableArchetypeReferencesUtil.getName(airl))
                        .setDescription(ReadableArchetypeReferencesUtil.getHTMLTooltip(airl))
                        .setIcon(Archetypes.getIcon(idArchetype))
                        .setObject(airl)
                        .createSelectableNode();
            }
        }
        return null;
    }

    public static SelectableNode<Object> getArchetypeElementRuleLineElementNode(ArchetypeElementInstantiationRuleLine aeirl, boolean onlyCDSDomain, ArchetypeReference ar)
            throws InternalErrorException, InstanceNotFoundException {
        ArchetypeElementVO archetypeElementVO = aeirl.getArchetypeElementRuleLineDefinitionElement().getValue();
        if(archetypeElementVO != null){
            ArchetypeReference arAux = aeirl.getArchetypeReference();
            if (ar == null || ar.equals(arAux)){
                String domainId = arAux.getIdDomain();
                if (!onlyCDSDomain || Domains.CDS_ID.equals(domainId)){
                    String name = EditorManager.getActiveGDLEditor().getGTName(aeirl.getGTCodeRuleLineElement().getValue());
                    if (name == null) {
                        name = "*EMPTY*";
                    }
                    name = name.length() > 30 ? name.substring(0, 30) + "..." : name;
                    return new SelectableNodeBuilder<Object>()
                            .setName(name)
                            .setDescription(ArchetypeReferences.getHTMLTooltip(archetypeElementVO, arAux))
                            .setIcon(getIcons(aeirl.getGTCodeRuleLineElement()))
                            .setObject(aeirl.getGTCodeRuleLineElement())
                            .createSelectableNode();
                }
            }
        }
        return null;
    }

    public static SelectableNode<Object> getCurrentDateTimeArchetypeElementRuleLineElementNode(GTCodeRuleLineElement currentDateTimeGTCodeRuleLineElement){
        String name = GDLEditorLanguageManager.getMessage("CurrentDateTime");
        return new SelectableNodeBuilder<Object>()
                .setName(name)
                .setIcon(OpenEHRDataValuesUI.getIcon(OpenEHRDataValues.DV_DATE_TIME))
                .setObject(currentDateTimeGTCodeRuleLineElement)
                .createSelectableNode();
    }

    private static GTCodeRuleLineElement getCurrentDateTimeGTCodeRuleLineElement(){
        ArchetypeElementInstantiationRuleLine aeirl = new ArchetypeElementInstantiationRuleLine(null);
        GTCodeRuleLineElement currentDateTimeGTCodeRuleLineElement = aeirl.getGTCodeRuleLineElement();
        aeirl.setArchetypeElementVO(ArchetypeElements.CURRENT_DATE_TIME);
        currentDateTimeGTCodeRuleLineElement.setValue(OpenEHRConst.CURRENT_DATE_TIME_ID);
        return currentDateTimeGTCodeRuleLineElement;
    }

    public static ImageIcon getIcons(GTCodeRuleLineElement gtCodeRuleLineElement) throws InstanceNotFoundException, InternalErrorException {
        RuleLine ruleLine = gtCodeRuleLineElement.getParentRuleLine();
        if (ruleLine instanceof ArchetypeElementInstantiationRuleLine){
            ArchetypeElementRuleLineDefinitionElement aerlde =
                    ((ArchetypeElementInstantiationRuleLine)ruleLine).getArchetypeElementRuleLineDefinitionElement();
            if (aerlde.getValue() != null){
                return getIconsArchetypeElement(aerlde);
            }
        }else if (ruleLine instanceof ArchetypeInstantiationRuleLine){
            ArchetypeReferenceRuleLineDefinitionElement arrlde =
                    ((ArchetypeInstantiationRuleLine)ruleLine).getArchetypeReferenceRuleLineDefinitionElement();
            if (arrlde.getValue() != null){
                return getIconsArchetypeReference(arrlde);
            }
        }
        return null;
    }

    public static ImageIcon getIconsArchetypeElement(ArchetypeElementRuleLineDefinitionElement aerlde) {
        String archetypeId = aerlde.getValue().getIdArchetype();
        String archReferenceRM = Archetypes.getEntryType(archetypeId);
        String archElementRM = aerlde.getValue().getRMType();
        MultipleIcon icons =
                new MultipleIcon(
                        new Icon[]{
                                DomainsUI.getGroupIconFromArchetypeReference(aerlde.getArchetypeReference()),
                                OpenEHRConstUI.getIcon(archReferenceRM),
                                OpenEHRDataValuesUI.getIcon(archElementRM)});
        return icons;
    }


    public static ImageIcon getIconsArchetypeReference(ArchetypeReferenceRuleLineDefinitionElement arrlde) throws InternalErrorException, InstanceNotFoundException {
        String archetypeId = arrlde.getValue().getIdArchetype();
        String archReferenceRM = Archetypes.getEntryType(archetypeId);
        MultipleIcon icons =
                new MultipleIcon(
                        new Icon[]{
                                DomainsUI.getGroupIconFromArchetypeReference(arrlde.getValue()),
                                OpenEHRConstUI.getIcon(archReferenceRM)});
        return icons;
    }

    public static SelectableNode<Object> getElementsInArchetypeNode(
            String idArchetype, String idTemplate, boolean singleSelection,
            boolean simplifiedTree, ArchetypeManager archetypeManager) throws InternalErrorException, InstanceNotFoundException {
        ArchetypeDTO archetypeVO = archetypeManager.getArchetypes().getCMElement(idArchetype);
        SelectableNode.SelectionMode selectionMode = getSelectionMode(singleSelection);
        SelectableNode<Object> rootNode = new SelectableNodeBuilder<Object>()
                .setName(idArchetype)
                .setDescription(idArchetype)
                .setSelectionMode(selectionMode)
                .setIcon(Archetypes.getIcon(archetypeVO.getId()))
                .createSelectableNode();
        Map<String, SelectableNode<Object>> rmNodes =
                new HashMap<String, SelectableNode<Object>>();

        Map<Object, SelectableNode<Object>> clusters =
                new HashMap<Object, SelectableNode<Object>>();

        SelectableNode<Object> nodoOrigen;
        Collection<ArchetypeElementVO> archetypeElementVOs = archetypeManager.getArchetypeElements().getArchetypeElementsVO(idArchetype, idTemplate);
        for (ArchetypeElementVO archetypeElementVO : archetypeElementVOs) {
            SelectableNode<Object> rmNode = ClusterNodesUtil.getRMNode(rootNode, rmNodes, archetypeElementVO.getPath());
            SelectableNode<Object> clusterNode =
                    ClusterNodesUtil.getClusterNode(
                            idTemplate,
                            archetypeElementVO.getParentId(),
                            rmNode, clusters,
                            singleSelection,
                            simplifiedTree,
                            archetypeManager);
            nodoOrigen = createElementNode(archetypeElementVO, selectionMode, archetypeManager);
            clusterNode.add(nodoOrigen);
        }
        return rootNode;
    }

    private static SelectableNode.SelectionMode getSelectionMode(boolean singleSelection) {
        return singleSelection ? SelectableNode.SelectionMode.SINGLE : SelectableNode.SelectionMode.MULTIPLE;
    }

    private static SelectableNode<Object> createElementNode(ArchetypeElementVO archetypeElementVO, SelectableNode.SelectionMode selectionMode, ArchetypeManager archetypeManager) {
        String name = archetypeManager.getArchetypeElements().getText(archetypeElementVO, UserConfigurationManager.instance().getLanguage());
        String desc = archetypeManager.getArchetypeElements().getDescription(archetypeElementVO, UserConfigurationManager.instance().getLanguage());
        return  new SelectableNodeBuilder<Object>()
                .setName(name)
                .setDescription(desc)
                .setSelectionMode(selectionMode)
                .setIcon(OpenEHRDataValuesUI.getIcon(archetypeElementVO.getRMType()))
                .setObject(archetypeElementVO)
                .createSelectableNode();
    }

    public static void addElementInstanceAttributesAndFunctionsToNode(RuleLineCollection ruleLineCollection, SelectableNode<Object> node, boolean onlyCDSDomain, ArchetypeReference ar)
            throws InstanceNotFoundException, InternalErrorException {
        for (RuleLine ruleLine : ruleLineCollection.getRuleLines()) {
            if (ruleLine instanceof ArchetypeElementInstantiationRuleLine){
                ArchetypeElementInstantiationRuleLine aeirl = (ArchetypeElementInstantiationRuleLine)ruleLine;
                if (aeirl.getArchetypeElement()!=null){
                    SelectableNode<Object> nodeAux =
                            getArchetypeElementRuleLineElementNode(aeirl, onlyCDSDomain, ar);
                    if(nodeAux!=null){
                        GTCodeRuleLineElement gtCodeRuleLineElement =
                                (GTCodeRuleLineElement)nodeAux.getObject();
                        addFieldsToNode(nodeAux, aeirl.getArchetypeElement().getRMType(), gtCodeRuleLineElement);
                        addFunctionsToNode(nodeAux, gtCodeRuleLineElement);
                        node.add(nodeAux);
                    }
                }
            }
            addElementInstanceAttributesAndFunctionsToNode(ruleLine.getChildrenRuleLines(), node, onlyCDSDomain, ar);
        }
    }

    public static void addFieldsToNode(SelectableNode<Object> node, String rmName, GTCodeRuleLineElement gtCodeRuleLineElement){
        node.setObject(null);
        String[] fieldNames =
                OpenEHRDataValuesUI.getFieldNames(rmName);
        for (String fieldName : fieldNames) {
            AttributeFunctionContainerNode attributeNode = new AttributeFunctionContainerNode(
                    gtCodeRuleLineElement,
                    fieldName);
            SelectableNode<Object> fieldNode =
                    new SelectableNodeBuilder<Object>()
                            .setName(fieldName)
                            .setIcon(GDLEditorImageUtil.OBJECT_ICON)
                            .setObject(attributeNode)
                            .createSelectableNode();
            node.add(fieldNode);
        }
    }

    public static void addFunctionsToNode(SelectableNode<?> node, GTCodeRuleLineElement gtCodeRuleLineElement){
        node.setObject(null);
        ArrayList<String> functionNames =
                OpenEHRDataValuesUI.getFunctionNames();
        for (String functionName : functionNames) {
            AttributeFunctionContainerNode functionNode = new AttributeFunctionContainerNode(
                    gtCodeRuleLineElement,
                    functionName);
            SelectableNode<Object> fieldNode =
                    new SelectableNodeBuilder<Object>()
                            .setName(functionName)
                            .setIcon(GDLEditorImageUtil.FUNCTION_ICON)
                            .setObject(functionNode)
                            .createSelectableNode();
            node.add(fieldNode);
        }
    }

    public static SelectableNode<Object> getSingleNodeAttributesAndFunctions(){
        String name = GDLEditorLanguageManager.getMessage("Attributes")+"/"+GDLEditorLanguageManager.getMessage("Functions");
        return new SelectableNodeBuilder<Object>()
                .setName(name)
                .setIcon(GDLEditorImageUtil.OBJECT_ICON)
                .createSelectableNode();
    }

    public static SelectableNode<Object> getNodeAttributesAndFunctions(GDLEditor gdlEditor, boolean onlyCDSDomain, ArchetypeReference ar) {
        RuleLineCollection definitionRuleLines = gdlEditor.getDefinitionRuleLines();
        SelectableNode<Object> root = getSingleNodeAttributesAndFunctions();
        SelectableNode<Object> elementsNode = getElementsNode();
        root.add(elementsNode);
        try {
            addElementInstanceAttributesAndFunctionsToNode(definitionRuleLines, elementsNode, onlyCDSDomain, ar);
        } catch (InstanceNotFoundException e) {
            ExceptionHandler.handle(e);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        }
        if (!onlyCDSDomain){
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

    public static SelectableNode<Object> getNodeAttributesAndFunctionsPredicate(){
        SelectableNode<Object> root = new SelectableNodeBuilder<Object>()
                .setName(GDLEditorLanguageManager.getMessage("Attributes"))
                .setIcon(GDLEditorImageUtil.OBJECT_ICON)
                .createSelectableNode();
        SelectableNode<Object> currentDateTimeNode = getCurrentTimeNodeWithAttributes();
        root.add(currentDateTimeNode);
        return root;
    }

    public static SelectableNode<Object> getNodeAttributesAndFunctions(String archetypteId, String templateId, ArchetypeManager archetypeManager){
        SelectableNode<Object> root =
                new SelectableNodeBuilder<Object>()
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
                        new SelectableNodeBuilder<Object>()
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

    public static SelectableNode<Object> getNodeGTCodes(Map<String, Term> termsMap, Collection<String> gtCodesToBeIgnored){
        SelectableNode<Object> root =
                new SelectableNodeBuilder<Object>()
                        .setName(GDLEditorLanguageManager.getMessage("LocalTerms"))
                        .setIcon(GDLEditorImageUtil.OBJECT_ICON)
                        .createSelectableNode();
        ArrayList<String> terms = new ArrayList<String>(termsMap.keySet());
        Collections.sort(terms);
        for (String gtCode : terms) {
            if (!gtCodesToBeIgnored.contains(gtCode)){
                String name = termsMap.get(gtCode).getText();
                String gtDesc = gtCode + (name!=null?" - " + name:"");
                SelectableNode<Object> nodeAux =
                        new SelectableNodeBuilder<Object>()
                                .setName(gtDesc)
                                .setIcon(GDLEditorImageUtil.OBJECT_ICON)
                                .setObject(gtCode)
                                .createSelectableNode();
                root.add(nodeAux);
            }
        }
        return root;
    }

    public static SelectableNode<Object> getNodeTerminologyIds(List<String> terminologyIds){
        SelectableNode<Object> root =
                new SelectableNodeBuilder<Object>()
                        .setName(GDLEditorLanguageManager.getMessage("Terminologies"))
                        .setIcon(OpenEHRImageUtil.TERMSET)
                        .createSelectableNode();
        for (String terminologyId : terminologyIds) {
            SelectableNode<Object> nodeAux =
                    new SelectableNodeBuilder<Object>()
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

    public static SelectableNode<Object> getElementsNode() {
        SelectableNode<Object> elementsNode =  new SelectableNodeBuilder<Object>()
                .setName(GDLEditorLanguageManager.getMessage("Elements"))
                .setIcon(GDLEditorImageUtil.FOLDER_OBJECT_ICON)
                .createSelectableNode();
        return elementsNode;
    }

    private static void addRuleNodes(LinkedHashMap<String, ReadableRule> renderableRules, SelectableNode<GTCodeRuleLineElement> rootNode, boolean showAttributes) {
        for (ReadableRule readableRule: renderableRules.values()) {
            GTCodeRuleLineElement gtCodeRuleLineElement = readableRule.getDefinitionRuleLine().getGTCodeRuleLineElement();
            String ruleName = readableRule.getTermDefinition().getTermText(readableRule.getGTCode());
            GTCodeRuleLineElement ruleObject = showAttributes ? null : gtCodeRuleLineElement;
            SelectableNode<GTCodeRuleLineElement> ruleNode =
                    new SelectableNodeBuilder()
                            .setName(ruleName)
                            .setIcon(CDSImageUtil.RULE_ICON)
                            .setObject(ruleObject)
                            .createSelectableNode();
            rootNode.add(ruleNode);
            if (showAttributes) {
                addFunctionsToNode(ruleNode, gtCodeRuleLineElement);
            }
        }
    }

    public static SelectableNode<GTCodeRuleLineElement> getRulesNode() {
        SelectableNode<GTCodeRuleLineElement> rulesNode =
                new SelectableNodeBuilder()
                        .setName(GDLEditorLanguageManager.getMessage("Rules"))
                        .setIcon(CDSImageUtil.RULE_ICON)
                        .setObject(null)
                        .createSelectableNode();
        return rulesNode;
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