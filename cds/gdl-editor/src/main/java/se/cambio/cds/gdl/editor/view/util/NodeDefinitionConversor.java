package se.cambio.cds.gdl.editor.view.util;

import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.cds.controller.session.data.ArchetypeReferences;
import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.model.Term;
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
import se.cambio.cds.view.swing.applicationobjects.DomainsUI;
import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.openehr.controller.session.data.ArchetypeElements;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.model.facade.terminology.vo.TerminologyNodeVO;
import se.cambio.openehr.util.*;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.view.trees.SelectableNode;
import se.cambio.openehr.view.trees.SelectableNodeBuilder;

import javax.swing.*;
import java.util.*;

public class NodeDefinitionConversor {

    public static SelectableNode<Object> getElementInstancesSelectionNodes(
            Collection<RuleLine> definitionRuleLines, boolean onlyCDSDomain, ArchetypeReference ar){
        SelectableNode<Object> root = new SelectableNodeBuilder<Object>()
                .setName(GDLEditorLanguageManager.getMessage("Definitions"))
                .setIcon(GDLEditorImageUtil.FOLDER_OBJECT_ICON)
                .createSelectableNode();
        SelectableNode<Object> elementsNode = new SelectableNodeBuilder<Object>()
                .setName(GDLEditorLanguageManager.getMessage("ElementInstances"))
                .setIcon(GDLEditorImageUtil.FOLDER_OBJECT_ICON)
                .createSelectableNode();
        root.add(elementsNode);
        addElementInstanceToNode(definitionRuleLines, elementsNode, onlyCDSDomain, ar);
        root.add(getArchetypeInstancesSelectionNodes(definitionRuleLines, onlyCDSDomain, ar));
        return root;
    }

    public static void addElementInstanceToNode(Collection<RuleLine> ruleLines, SelectableNode<Object> node, boolean onlyCDSDomain, ArchetypeReference ar){
        for (RuleLine ruleLine : ruleLines) {
            if (ruleLine instanceof ArchetypeElementInstantiationRuleLine){
                SelectableNode<Object> nodeAux =
                        getArchetypeElementRuleLineElementNode((ArchetypeElementInstantiationRuleLine)ruleLine, onlyCDSDomain, ar);
                if(nodeAux != null){
                    node.add(nodeAux);
                }
            }
            addElementInstanceToNode(ruleLine.getChildrenRuleLines(), node, onlyCDSDomain, ar);
        }
    }

    public static SelectableNode<Object> getArchetypeInstancesSelectionNodes(Collection<RuleLine> definitionRuleLines, boolean onlyCDSDomain, ArchetypeReference ar){
        SelectableNode<Object> root =  new SelectableNodeBuilder<Object>()
                .setName(GDLEditorLanguageManager.getMessage("ArchetypeInstances"))
                .setIcon(GDLEditorImageUtil.FOLDER_OBJECT_ICON)
                .createSelectableNode();
        for (RuleLine ruleLine : definitionRuleLines) {
            if (ruleLine instanceof ArchetypeInstantiationRuleLine){
                SelectableNode<Object> node = getArchetypeElementRuleLineElementNode((ArchetypeInstantiationRuleLine)ruleLine, onlyCDSDomain, ar);
                if (node!=null){
                    root.add(node);
                }
            }
        }
        return root;
    }

    public static SelectableNode<Object> getArchetypeElementRuleLineElementNode(ArchetypeInstantiationRuleLine airl, boolean onlyCDSDomain, ArchetypeReference ar){
        ArchetypeReference arAux = airl.getArchetypeReference();
        if (arAux != null){
            String idArchetype = arAux.getIdArchetype();
            //String idTemplate = airl.getArchetypeReference().getArchetypeId();
            if ((!onlyCDSDomain || Domains.CDS_ID.equals(airl.getArchetypeReference().getIdDomain()) && (ar == null || ar.equals(airl.getArchetypeReference())))){
                return new SelectableNodeBuilder<Object>()
                        .setName(ReadableArchetypeReferencesUtil.getName(airl))
                        .setDescription(ReadableArchetypeReferencesUtil.getHTMLTooltip(airl))
                        .setIcon(Archetypes.getIcon(idArchetype))
                        .createSelectableNode();
            }
        }
        return null;
    }

    public static SelectableNode<Object> getArchetypeElementRuleLineElementNode(ArchetypeElementInstantiationRuleLine aeirl, boolean onlyCDSDomain, ArchetypeReference ar){
        ArchetypeElementVO archetypeElementVO = aeirl.getArchetypeElementRuleLineDefinitionElement().getValue();
        if(archetypeElementVO!=null){
            ArchetypeReference arAux = aeirl.getArchetypeReference();
            if (ar==null || ar.equals(arAux)){
                String domainId = arAux.getIdDomain();
                if (!onlyCDSDomain || Domains.CDS_ID.equals(domainId)){
                    String name = EditorManager.getActiveGDLEditor().getGTName(aeirl.getGTCodeRuleLineElement().getValue());
                    if (name==null){
                        name = "*EMPTY*";
                    }
                    name = name.length()>30?name.substring(0, 30)+"...":name;
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
        aeirl.setTermDefinition(EditorManager.getActiveGDLEditor().getCurrentTermDefinition());
        GTCodeRuleLineElement currentDateTimeGTCodeRuleLineElement = aeirl.getGTCodeRuleLineElement();
        aeirl.setArchetypeElementVO(ArchetypeElements.CURRENT_DATE_TIME);
        currentDateTimeGTCodeRuleLineElement.setValue(OpenEHRConst.CURRENT_DATE_TIME_ID);
        return currentDateTimeGTCodeRuleLineElement;
    }

    public static ImageIcon getIcons(GTCodeRuleLineElement gtCodeRuleLineElement){
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

    public static ImageIcon getIconsArchetypeElement(ArchetypeElementRuleLineDefinitionElement aerlde){
        String archReferenceRM = Archetypes.getArchetypeDTO(aerlde.getValue().getIdArchetype()).getRMName();
        String archElementRM = aerlde.getValue().getRMType();
        MultipleIcon icons =
                new MultipleIcon(
                        new Icon[]{
                                DomainsUI.getGroupIconFromArchetypeReference(aerlde.getArchetypeReference()),
                                OpenEHRConstUI.getIcon(archReferenceRM),
                                OpenEHRDataValuesUI.getIcon(archElementRM)});
        return icons;
    }


    public static ImageIcon getIconsArchetypeReference(ArchetypeReferenceRuleLineDefinitionElement arrlde){

        String archReferenceRM = Archetypes.getArchetypeDTO(arrlde.getValue().getIdArchetype()).getRMName();
        MultipleIcon icons =
                new MultipleIcon(
                        new Icon[]{
                                DomainsUI.getGroupIconFromArchetypeReference(arrlde.getValue()),
                                OpenEHRConstUI.getIcon(archReferenceRM)});
        return icons;
    }

    public static SelectableNode<Object> getElementsInArchetypeNode(
            String idArchetype, String idTemplate, boolean singleSelection, boolean simplifiedTree){
        ArchetypeDTO archetypeVO = Archetypes.getArchetypeDTO(idArchetype);
        SelectableNode.SelectionMode selectionMode = getSelectionMode(singleSelection);
        SelectableNode<Object> rootNode = new SelectableNodeBuilder<Object>()
                .setName(archetypeVO.getName())
                .setDescription(archetypeVO.getDescription())
                .setSelectionMode(selectionMode)
                .setIcon(Archetypes.getIcon(archetypeVO.getArchetypeId()))
                .createSelectableNode();
        Map<String, SelectableNode<Object>> rmNodes =
                new HashMap<String, SelectableNode<Object>>();

        Map<Object, SelectableNode<Object>> clusters =
                new HashMap<Object, SelectableNode<Object>>();

        SelectableNode<Object> nodoOrigen;
        Collection<ArchetypeElementVO> archetypeElementVOs = ArchetypeElements.getArchetypeElementsVO(idArchetype, idTemplate);
        for (ArchetypeElementVO archetypeElementVO : archetypeElementVOs) {
            SelectableNode<Object> rmNode = ClusterNodesUtil.getRMNode(rootNode, rmNodes, archetypeElementVO.getPath());
            SelectableNode<Object> clusterNode =
                    ClusterNodesUtil.getClusterNode(
                            idTemplate,
                            archetypeElementVO.getIdParent(),
                            rmNode, clusters,
                            singleSelection,
                            simplifiedTree);
            nodoOrigen = createElementNode(archetypeElementVO, selectionMode);
            clusterNode.add(nodoOrigen);
        }
        return rootNode;
    }

    private static SelectableNode.SelectionMode getSelectionMode(boolean singleSelection) {
        return singleSelection ? SelectableNode.SelectionMode.SINGLE : SelectableNode.SelectionMode.MULTIPLE;
    }

    private static SelectableNode<Object> createElementNode(ArchetypeElementVO archetypeElementVO, SelectableNode.SelectionMode selectionMode){
        String name = ArchetypeElements.getText(archetypeElementVO, UserConfigurationManager.getLanguage());
        String desc = ArchetypeElements.getDescription(archetypeElementVO, UserConfigurationManager.getLanguage());
        return  new SelectableNodeBuilder<Object>()
                .setName(name)
                .setDescription(desc)
                .setSelectionMode(selectionMode)
                .setIcon(OpenEHRDataValuesUI.getIcon(archetypeElementVO.getRMType()))
                .setObject(archetypeElementVO)
                .createSelectableNode();
    }

    public static void addElementInstanceAttributesAndFunctionsToNode(Collection<RuleLine> ruleLines, SelectableNode<Object> node, boolean onlyCDSDomain, ArchetypeReference ar){
        for (RuleLine ruleLine : ruleLines) {
            if (ruleLine instanceof ArchetypeElementInstantiationRuleLine){
                ArchetypeElementInstantiationRuleLine aeirl = (ArchetypeElementInstantiationRuleLine)ruleLine;
                if (aeirl.getArchetypeElement()!=null){
                    SelectableNode<Object> nodeAux =
                            getArchetypeElementRuleLineElementNode(aeirl, onlyCDSDomain, ar);
                    if(nodeAux!=null){
                        GTCodeRuleLineElement gtCodeRuleLineElement =
                                (GTCodeRuleLineElement)nodeAux.getObject();
                        addFieldsToNode(nodeAux, aeirl.getArchetypeElement().getRMType(), gtCodeRuleLineElement);
                        addFuntionsToNode(nodeAux, aeirl.getArchetypeElement().getRMType(), gtCodeRuleLineElement);
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

    public static void addFuntionsToNode(SelectableNode<Object> node, String rmName, GTCodeRuleLineElement gtCodeRuleLineElement){
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

    public static SelectableNode<Object> getNodeAttributesAndFunctions(Collection<RuleLine> definitionRuleLines, boolean onlyCDSDomain, ArchetypeReference ar){
        SelectableNode<Object> root = getSingleNodeAttributesAndFunctions();
        addElementInstanceAttributesAndFunctionsToNode(definitionRuleLines, root, onlyCDSDomain, ar);
        if (!onlyCDSDomain){
            GTCodeRuleLineElement currentDateTimeGTCodeRuleLineElement = getCurrentDateTimeGTCodeRuleLineElement();
            SelectableNode<Object> currentDateTimeNode =
                    getCurrentDateTimeArchetypeElementRuleLineElementNode(currentDateTimeGTCodeRuleLineElement);
            addFieldsToNode(currentDateTimeNode, OpenEHRDataValues.DV_DATE_TIME, getCurrentDateTimeGTCodeRuleLineElement());
            root.add(currentDateTimeNode);
        }
        root.add(getArchetypeInstancesSelectionNodes(definitionRuleLines, onlyCDSDomain, ar));
        return root;
    }

    public static SelectableNode<Object> getNodeAttributesAndFunctionsPredicate(){
        SelectableNode<Object> root = new SelectableNodeBuilder<Object>()
                .setName(GDLEditorLanguageManager.getMessage("Attributes"))
                .setIcon(GDLEditorImageUtil.OBJECT_ICON)
                .createSelectableNode();
        GTCodeRuleLineElement currentDateTimeGTCodeRuleLineElement = getCurrentDateTimeGTCodeRuleLineElement();
        SelectableNode<Object> currentDateTimeNode =
                getCurrentDateTimeArchetypeElementRuleLineElementNode(currentDateTimeGTCodeRuleLineElement);
        addFieldsToNode(currentDateTimeNode, OpenEHRDataValues.DV_DATE_TIME, getCurrentDateTimeGTCodeRuleLineElement());
        root.add(currentDateTimeNode);
        return root;
    }
    public static SelectableNode<Object> getNodeAttributesAndFunctions(String archetypteId, String templateId){
        SelectableNode<Object> root =
                new SelectableNodeBuilder<Object>()
                        .setName(GDLEditorLanguageManager.getMessage("Attributes"))
                        .setIcon(GDLEditorImageUtil.OBJECT_ICON)
                        .createSelectableNode();
        Collection<ArchetypeElementVO> archetypeElementVOs = ArchetypeElements.getArchetypeElementsVO(archetypteId, templateId);
        for (ArchetypeElementVO archetypeElementVO : archetypeElementVOs) {
            SelectableNode<Object> elementNode = createElementNode(archetypeElementVO, SelectableNode.SelectionMode.SINGLE);
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

    public static SelectableNode<Object> getNodeTerminologyIds(){
        SelectableNode<Object> root =
                new SelectableNodeBuilder<Object>()
                        .setName(GDLEditorLanguageManager.getMessage("Terminologies"))
                        .setIcon(GDLEditorImageUtil.ONTOLOGY_ICON)
                        .createSelectableNode();
        try{
            for (String terminologyId : OpenEHRSessionManager.getTerminologyFacadeDelegate().getSupportedTerminologies()) {
                SelectableNode<Object> nodeAux =
                        new SelectableNodeBuilder<Object>()
                                .setName(terminologyId)
                                .setIcon(GDLEditorImageUtil.ONTOLOGY_ICON)
                                .setObject(terminologyId)
                                .createSelectableNode();
                root.add(nodeAux);
            }
        }catch (InternalErrorException e){
            ExceptionHandler.handle(e);
        }
        return root;
    }

    public static SelectableNode<Object> getNodeAllTerminologyCodes(String terminologyId, Collection<String> selectedCodes){
        SelectableNode<Object> root =
                new SelectableNodeBuilder<Object>()
                        .setName(terminologyId)
                        .setIcon(GDLEditorImageUtil.ONTOLOGY_ICON)
                        .createSelectableNode();
        List<TerminologyNodeVO> nodes = null;
        try{
            nodes = OpenEHRSessionManager.getTerminologyFacadeDelegate().retrieveAll(terminologyId, OpenEHRDataValuesUI.getLanguageCodePhrase());
        }catch (InternalErrorException e){
            ExceptionHandler.handle(e);
        }
        if (nodes!=null){
            Collections.sort(nodes, new Comparator<TerminologyNodeVO>() {
                @Override
                public int compare(TerminologyNodeVO o1, TerminologyNodeVO o2) {
                    return o1.getValue().getDefiningCode().getCodeString().compareTo(o2.getValue().getDefiningCode().getCodeString());
                }
            });
            for (TerminologyNodeVO node : nodes) {
                root.add(getSelectableNodeTerminologyCodes(node, selectedCodes));
            }
        }
        return root;
    }

    public static SelectableNode<Object> getSelectableNodeTerminologyCodes(TerminologyNodeVO node, Collection <String> selectedCodes){
        String code = node.getValue().getDefiningCode().getCodeString();
        String name = node.getValue().getValue() +" ("+code+")";
        boolean selected = selectedCodes!=null && selectedCodes.contains(code);
        SelectableNode<Object> selectableNode =
                new SelectableNodeBuilder<Object>()
                        .setName(name)
                        .setIcon(GDLEditorImageUtil.ONTOLOGY_ICON)
                        .setObject(node.getValue())
                        .setSelectionMode(SelectableNode.SelectionMode.MULTIPLE)
                        .setSelectionPropagationMode(SelectableNode.SelectionPropagationMode.NONE)
                        .setSelected(selected)
                        .createSelectableNode();
        for (TerminologyNodeVO node2 : node.getChildren()) {
            selectableNode.add(getSelectableNodeTerminologyCodes(node2, selectedCodes));
        }
        return selectableNode;
    }

    public static boolean selectCodesWith(SelectableNode<?> node, Object object, boolean multiple){
        if (node.getObject() instanceof DvCodedText){
            if (object.equals(((DvCodedText)node.getObject()).getDefiningCode().getCodeString())){
                node.setSelected(true);
                node.stateChange(node);
                if (!multiple){
                    return true;
                }
            }
        }
        Enumeration<?>  e = node.getAllchildren();
        while(e.hasMoreElements()){
            Object nodeObj = e.nextElement();
            if (nodeObj instanceof SelectableNode){
                boolean found = selectCodesWith((SelectableNode)nodeObj, object, multiple);
                if (found && !multiple){
                    return true;
                }
            }
        }
        return false;
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