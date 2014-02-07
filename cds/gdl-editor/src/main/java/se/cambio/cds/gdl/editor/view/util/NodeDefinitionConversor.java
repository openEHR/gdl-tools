package se.cambio.cds.gdl.editor.view.util;

import org.openehr.rm.datatypes.text.CodePhrase;
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
import se.cambio.openehr.view.trees.SelectableNodeWithIcon;

import javax.swing.*;
import java.util.*;

public class NodeDefinitionConversor {

    public static SelectableNode<Object> getElementInstancesSelectionNodes(
            Collection<RuleLine> definitionRuleLines, boolean onlyCDSDomain){
        SelectableNode<Object> root = new SelectableNodeWithIcon<Object>(
                GDLEditorLanguageManager.getMessage("Definitions"),null, true, false, GDLEditorImageUtil.FOLDER_OBJECT_ICON);
        SelectableNode<Object> elementsNode = new SelectableNodeWithIcon<Object>(
                GDLEditorLanguageManager.getMessage("ElementInstances"),null, true, false, GDLEditorImageUtil.FOLDER_OBJECT_ICON);
        root.add(elementsNode);
        addElementInstanceToNode(definitionRuleLines, elementsNode, onlyCDSDomain);
	/*
	if (!onlyCDSDomain){
	    elementsNode.add(getCurrentDateTimeArchetypeElementRuleLineElementNode(getCurrentDateTimeGTCodeRuleLineElement()));
	}*/
        root.add(getArchetypeInstancesSelectionNodes(definitionRuleLines, onlyCDSDomain));
        return root;
    }

    public static void addElementInstanceToNode(Collection<RuleLine> ruleLines, SelectableNode<Object> node, boolean onlyCDSDomain){
        for (RuleLine ruleLine : ruleLines) {
            if (ruleLine instanceof ArchetypeElementInstantiationRuleLine){
                SelectableNode<Object> nodeAux =
                        getArchetypeElementRuleLineElementNode((ArchetypeElementInstantiationRuleLine)ruleLine, onlyCDSDomain);
                if(nodeAux!=null){
                    node.add(nodeAux);
                }
            }
            addElementInstanceToNode(ruleLine.getChildrenRuleLines(), node, onlyCDSDomain);
        }
    }

    public static SelectableNode<Object> getArchetypeInstancesSelectionNodes(Collection<RuleLine> definitionRuleLines, boolean onlyCDSDomain){
        SelectableNode<Object> root = new SelectableNodeWithIcon<Object>(
                GDLEditorLanguageManager.getMessage("ArchetypeInstances"),null, true, false, GDLEditorImageUtil.FOLDER_OBJECT_ICON);
        for (RuleLine ruleLine : definitionRuleLines) {
            if (ruleLine instanceof ArchetypeInstantiationRuleLine){
                SelectableNode<Object> node = getArchetypeElementRuleLineElementNode((ArchetypeInstantiationRuleLine)ruleLine, onlyCDSDomain);
                if (node!=null){
                    root.add(node);
                }
            }
        }
        return root;
    }

    public static SelectableNode<Object> getArchetypeElementRuleLineElementNode(ArchetypeInstantiationRuleLine airl, boolean onlyCDSDomain){
        ArchetypeReference ar = airl.getArchetypeReference();
        if (ar!=null){
            String idArchetype = ar.getIdArchetype();
            //String idTemplate = airl.getArchetypeReference().getIdArchetype();
            if (!onlyCDSDomain || Domains.CDS_ID.equals(airl.getArchetypeReference().getIdDomain())){
                return new SelectableNodeWithIcon<Object>(
                        ReadableArchetypeReferencesUtil.getName(airl),
                        airl,
                        true,
                        false,
                        Archetypes.getIcon(idArchetype),
                        ReadableArchetypeReferencesUtil.getHTMLTooltip(airl));
            }
        }
        return null;
    }

    public static SelectableNode<Object> getArchetypeElementRuleLineElementNode(ArchetypeElementInstantiationRuleLine aeirl, boolean onlyCDSDomain){
        ArchetypeElementVO archetypeElementVO = aeirl.getArchetypeElementRuleLineDefinitionElement().getValue();
        if(archetypeElementVO!=null){
            ArchetypeReference ar = aeirl.getArchetypeReference();
            String domainId = ar.getIdDomain();
            if (!onlyCDSDomain || Domains.CDS_ID.equals(domainId)){
                String name = EditorManager.getActiveGDLEditor().getGTName(aeirl.getGTCodeRuleLineElement().getValue());
                if (name==null){
                    name = "*EMPTY*";
                }
                name = name.length()>30?name.substring(0, 30)+"...":name;
                return new SelectableNodeWithIcon<Object>(
                        name,
                        aeirl.getGTCodeRuleLineElement(),
                        true,
                        false,
                        getIcons(aeirl.getGTCodeRuleLineElement()),
                        ArchetypeReferences.getHTMLTooltip(archetypeElementVO, ar));
            }
        }
        return null;
    }

    public static SelectableNode<Object> getCurrentDateTimeArchetypeElementRuleLineElementNode(GTCodeRuleLineElement currentDateTimeGTCodeRuleLineElement){
        String name = GDLEditorLanguageManager.getMessage("CurrentDateTime");
        return new SelectableNodeWithIcon<Object>(
                name,
                currentDateTimeGTCodeRuleLineElement,
                true,
                false,
                OpenEHRDataValuesUI.getIcon(OpenEHRDataValues.DV_DATE_TIME));
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
            if (aerlde.getValue()!=null){
                return getIconsArchetypeElement(aerlde);
            }
        }else if (ruleLine instanceof ArchetypeInstantiationRuleLine){
            ArchetypeReferenceRuleLineDefinitionElement arrlde =
                    ((ArchetypeInstantiationRuleLine)ruleLine).getArchetypeReferenceRuleLineDefinitionElement();
            if (arrlde.getValue()!=null){
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
        SelectableNode<Object> rootNode = new SelectableNodeWithIcon<Object>(
                archetypeVO.getName(),
                null, singleSelection, false,
                Archetypes.getIcon(archetypeVO.getIdArchetype()),
                archetypeVO.getDescription());

        Map<String, SelectableNode<Object>> rmNodes =
                new HashMap<String, SelectableNode<Object>>();

        Map<Object, SelectableNode<Object>> clusters =
                new HashMap<Object, SelectableNode<Object>>();

        SelectableNode<Object> nodoOrigen = null;
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
            nodoOrigen = createElementNode(archetypeElementVO, singleSelection);
            clusterNode.add(nodoOrigen);
        }
        return rootNode;
    }

    private static SelectableNodeWithIcon<Object> createElementNode(ArchetypeElementVO archetypeElementVO, boolean singleSelection){
        return new SelectableNodeWithIcon<Object>(
                archetypeElementVO.getName(),archetypeElementVO,singleSelection, false,
                OpenEHRDataValuesUI.getIcon(archetypeElementVO.getRMType()), archetypeElementVO.getDescription());
    }

    public static void addElementInstanceAttributesAndFunctionsToNode(Collection<RuleLine> ruleLines, SelectableNode<Object> node, boolean onlyCDSDomain){
        for (RuleLine ruleLine : ruleLines) {
            if (ruleLine instanceof ArchetypeElementInstantiationRuleLine){
                ArchetypeElementInstantiationRuleLine aeirl = (ArchetypeElementInstantiationRuleLine)ruleLine;
                if (aeirl.getArchetypeElement()!=null){
                    SelectableNode<Object> nodeAux =
                            getArchetypeElementRuleLineElementNode(aeirl, onlyCDSDomain);
                    if(nodeAux!=null){
                        GTCodeRuleLineElement gtCodeRuleLineElement =
                                (GTCodeRuleLineElement)nodeAux.getObject();
                        addFieldsToNode(nodeAux, aeirl.getArchetypeElement().getRMType(), gtCodeRuleLineElement);
                        addFuntionsToNode(nodeAux, aeirl.getArchetypeElement().getRMType(), gtCodeRuleLineElement);
                        node.add(nodeAux);
                    }
                }
            }
            addElementInstanceAttributesAndFunctionsToNode(ruleLine.getChildrenRuleLines(), node, onlyCDSDomain);
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
            SelectableNodeWithIcon<Object> fieldNode =
                    new SelectableNodeWithIcon<Object>(
                            fieldName,
                            attributeNode,
                            true, false, GDLEditorImageUtil.OBJECT_ICON);
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
            SelectableNodeWithIcon<Object> fieldNode =
                    new SelectableNodeWithIcon<Object>(
                            functionName,
                            functionNode,
                            true, false, GDLEditorImageUtil.FUNCTION_ICON);
            node.add(fieldNode);
        }
    }

    public static SelectableNode<Object> getSingleNodeAttributesAndFunctions(){
        return new SelectableNodeWithIcon<Object>(GDLEditorLanguageManager.getMessage("Attributes")+"/"+GDLEditorLanguageManager.getMessage("Functions"), null, true, false, GDLEditorImageUtil.OBJECT_ICON);
    }

    public static SelectableNode<Object> getNodeAttributesAndFunctions(Collection<RuleLine> definitionRuleLines, boolean onlyCDSDomain){
        SelectableNode<Object> root = getSingleNodeAttributesAndFunctions();
        addElementInstanceAttributesAndFunctionsToNode(definitionRuleLines, root, onlyCDSDomain);
        if (!onlyCDSDomain){
            GTCodeRuleLineElement currentDateTimeGTCodeRuleLineElement = getCurrentDateTimeGTCodeRuleLineElement();
            SelectableNode<Object> currentDateTimeNode =
                    getCurrentDateTimeArchetypeElementRuleLineElementNode(currentDateTimeGTCodeRuleLineElement);
            addFieldsToNode(currentDateTimeNode, OpenEHRDataValues.DV_DATE_TIME, getCurrentDateTimeGTCodeRuleLineElement());
            root.add(currentDateTimeNode);
        }
        root.add(getArchetypeInstancesSelectionNodes(definitionRuleLines, onlyCDSDomain));
        return root;
    }

    public static SelectableNode<Object> getNodeAttributesAndFunctionsPredicate(){
        SelectableNode<Object> root = new SelectableNodeWithIcon<Object>(GDLEditorLanguageManager.getMessage("Attributes"), null, true, false, GDLEditorImageUtil.OBJECT_ICON);
        GTCodeRuleLineElement currentDateTimeGTCodeRuleLineElement = getCurrentDateTimeGTCodeRuleLineElement();
        SelectableNode<Object> currentDateTimeNode =
                getCurrentDateTimeArchetypeElementRuleLineElementNode(currentDateTimeGTCodeRuleLineElement);
        addFieldsToNode(currentDateTimeNode, OpenEHRDataValues.DV_DATE_TIME, getCurrentDateTimeGTCodeRuleLineElement());
        root.add(currentDateTimeNode);
        return root;
    }
    public static SelectableNode<Object> getNodeAttributesAndFunctions(String archetypteId, String templateId){
        SelectableNode<Object> root =
                new SelectableNodeWithIcon<Object>(GDLEditorLanguageManager.getMessage("Attributes"), null, true, false, GDLEditorImageUtil.OBJECT_ICON);
        Collection<ArchetypeElementVO> archetypeElementVOs = ArchetypeElements.getArchetypeElementsVO(archetypteId, templateId);
        for (ArchetypeElementVO archetypeElementVO : archetypeElementVOs) {
            SelectableNode<Object> elementNode = createElementNode(archetypeElementVO, true);
            String[] fieldNames =
                    OpenEHRDataValuesUI.getFieldNames(archetypeElementVO.getRMType());
            for (String fieldName : fieldNames) {
                SelectableNodeWithIcon<Object> fieldNode =
                        new SelectableNodeWithIcon<Object>(
                                fieldName,
                                new PredicateAttributeVO(archetypeElementVO, fieldName),
                                true, false, GDLEditorImageUtil.OBJECT_ICON);
                elementNode.add(fieldNode);
            }
            root.add(elementNode);
        }
        return root;
    }

    public static SelectableNode<Object> getNodeGTCodes(Map<String, Term> termsMap, Collection<String> gtCodesToBeIgnored){
        SelectableNodeWithIcon<Object> root =
                new SelectableNodeWithIcon<Object>(GDLEditorLanguageManager.getMessage("LocalTerms"), null, true, false, GDLEditorImageUtil.OBJECT_ICON);
        ArrayList<String> terms = new ArrayList<String>(termsMap.keySet());
        Collections.sort(terms);
        for (String gtCode : terms) {
            if (!gtCodesToBeIgnored.contains(gtCode)){
                String name = termsMap.get(gtCode).getText();
                String gtDesc = gtCode + (name!=null?" - " + name:"");
                SelectableNodeWithIcon<Object> nodeAux =
                        new SelectableNodeWithIcon<Object>(gtDesc, gtCode, true, false, GDLEditorImageUtil.OBJECT_ICON);
                root.add(nodeAux);
            }
        }
        return root;
    }

    public static SelectableNode<Object> getNodeTerminologyIds(){
        SelectableNodeWithIcon<Object> root =
                new SelectableNodeWithIcon<Object>(GDLEditorLanguageManager.getMessage("Terminologies"), null, true, false, GDLEditorImageUtil.ONTOLOGY_ICON);
        try{
            for (String terminologyId : OpenEHRSessionManager.getTerminologyFacadeDelegate().getSupportedTerminologies()) {
                SelectableNodeWithIcon<Object> nodeAux =
                        new SelectableNodeWithIcon<Object>(terminologyId, terminologyId, true, false, GDLEditorImageUtil.ONTOLOGY_ICON);
                root.add(nodeAux);
            }
        }catch (InternalErrorException e){
            ExceptionHandler.handle(e);
        }
        return root;
    }

    public static SelectableNode<Object> getNodeAllTerminologyCodes(String terminologyId, Collection<String> selectedCodes){
        SelectableNodeWithIcon<Object> root =
                new SelectableNodeWithIcon<Object>(terminologyId, null, true, false, GDLEditorImageUtil.ONTOLOGY_ICON);
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
        SelectableNodeWithIcon<Object> selectableNode =
                new SelectableNodeWithIcon<Object>(node.getValue().getValue() +" ("+code+")", node.getValue().getDefiningCode(), false, selectedCodes!=null && selectedCodes.contains(code), GDLEditorImageUtil.ONTOLOGY_ICON);
        selectableNode.setHierarchySelection(false);
        for (TerminologyNodeVO node2 : node.getChildren()) {
            selectableNode.add(getSelectableNodeTerminologyCodes(node2, selectedCodes));
        }
        return selectableNode;
    }

    public static boolean selectCodesWith(SelectableNode<?> node, Object object, boolean multiple){
        if (node.getObject() instanceof CodePhrase){
            if (object.equals(((CodePhrase)node.getObject()).getCodeString())){
                node.setSelected(true);
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