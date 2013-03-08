package se.cambio.cds.gdl.editor.view.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.LanguageManager;
import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeElementInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeElementRuleLineDefinitionElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeReferenceRuleLineDefinitionElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.GTCodeRuleLineElement;
import se.cambio.cds.gdl.model.readable.util.ReadableArchetypeReferencesUtil;
import se.cambio.cds.model.archetype.dto.ArchetypeDTO;
import se.cambio.cds.model.facade.execution.vo.ArchetypeReference;
import se.cambio.cds.openehr.model.archetypeelement.vo.ArchetypeElementVO;
import se.cambio.cds.openehr.util.OpenEHRConst;
import se.cambio.cds.openehr.util.OpenEHRDataValuesUI;
import se.cambio.cds.openehr.view.applicationobjects.AggregationFunctionsUI;
import se.cambio.cds.openehr.view.applicationobjects.ArchetypeElements;
import se.cambio.cds.openehr.view.applicationobjects.Archetypes;
import se.cambio.cds.openehr.view.applicationobjects.DomainsUI;
import se.cambio.cds.openehr.view.trees.SelectableNode;
import se.cambio.cds.openehr.view.trees.SelectableNodeWithIcon;
import se.cambio.cds.ts.Node;
import se.cambio.cds.ts.UnsupportedLanguageException;
import se.cambio.cds.ts.UnsupportedTerminologyException;
import se.cambio.cds.util.CDSTerminologyService;
import se.cambio.cds.util.Domains;
import se.cambio.cds.util.OpenEHRDataValues;
import se.cambio.cds.util.handlers.ExceptionHandler;

public class NodeDefinitionConversor {

    public static SelectableNode<Object> getElementInstancesSelectionNodes(
	    Collection<RuleLine> definitionRuleLines, boolean onlyCDSDomain){
	SelectableNode<Object> root = new SelectableNodeWithIcon<Object>(
		LanguageManager.getMessage("Definitions"),null, true, false, GDLEditorImageUtil.FOLDER_OBJECT_ICON);
	SelectableNode<Object> elementsNode = new SelectableNodeWithIcon<Object>(
		LanguageManager.getMessage("ElementInstances"),null, true, false, GDLEditorImageUtil.FOLDER_OBJECT_ICON);
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
		LanguageManager.getMessage("ArchetypeInstances"),null, true, false, GDLEditorImageUtil.FOLDER_OBJECT_ICON);
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
		name = name.length()>30?name.substring(0, 30)+"...":name;
		return new SelectableNodeWithIcon<Object>(
			name,
			aeirl.getGTCodeRuleLineElement(),
			true, 
			false, 
			getIcons(aeirl.getGTCodeRuleLineElement()),
			ArchetypeElements.getHTMLTooltip(archetypeElementVO, ar));
	    }
	}
	return null;
    }

    public static SelectableNode<Object> getCurrentDateTimeArchetypeElementRuleLineElementNode(GTCodeRuleLineElement currentDateTimeGTCodeRuleLineElement){
	String name = LanguageManager.getMessage("CurrentDateTime");
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
	String af = aerlde.getAggregationFunction();
	String archReferenceRM = Archetypes.getArchetypeVO(aerlde.getValue().getIdArchetype()).getRMName();
	String archElementRM = aerlde.getValue().getRMType();
	MultipleIcon icons = 
		new MultipleIcon(
			new Icon[]{
				DomainsUI.getGroupIconFromArchetypeReference(aerlde.getArchetypeReference()),
				OpenEHRConst.getIcon(archReferenceRM),
				AggregationFunctionsUI.getIcon(af),
				OpenEHRDataValuesUI.getIcon(archElementRM)});
	return icons;
    }


    public static ImageIcon getIconsArchetypeReference(ArchetypeReferenceRuleLineDefinitionElement arrlde){
	String af = arrlde.getAggregationFunction();

	String archReferenceRM = Archetypes.getArchetypeVO(arrlde.getValue().getIdArchetype()).getRMName();
	MultipleIcon icons = 
		new MultipleIcon(
			new Icon[]{
				DomainsUI.getGroupIconFromArchetypeReference(arrlde.getValue()),
				OpenEHRConst.getIcon(archReferenceRM),
				AggregationFunctionsUI.getIcon(af)});
	return icons;
    }

    public static SelectableNode<Object> getElementsInArchetypeNode(String idArchetype, String idTemplate){
	ArchetypeDTO archetypeVO = Archetypes.getArchetypeVO(idArchetype);
	SelectableNode<Object> rootNode = new SelectableNodeWithIcon<Object>(
		archetypeVO.getName(),
		null, true, false, 
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
	    SelectableNode<Object> clusterNode = ClusterNodesUtil.getClusterNode(idTemplate, archetypeElementVO.getIdParent(), rmNode, clusters);
	    nodoOrigen = createElementNode(archetypeElementVO);
	    clusterNode.add(nodoOrigen);
	}
	return rootNode;
    }

    private static SelectableNodeWithIcon<Object> createElementNode(ArchetypeElementVO archetypeElementVO){
	return new SelectableNodeWithIcon<Object>(
		archetypeElementVO.getName(),archetypeElementVO,true, false, 
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
				(GTCodeRuleLineElement)nodeAux.getObjeto();
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
	return new SelectableNodeWithIcon<Object>(LanguageManager.getMessage("Attributes")+"/"+LanguageManager.getMessage("Functions"), null, true, false, GDLEditorImageUtil.OBJECT_ICON);
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

    public static SelectableNode<Object> getNodeGTCodes(Map<String, Term> termsMap, Collection<String> gtCodesToBeIgnored){
	SelectableNodeWithIcon<Object> root = 
		new SelectableNodeWithIcon<Object>(LanguageManager.getMessage("LocalTerms"), null, true, false, GDLEditorImageUtil.OBJECT_ICON);
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
		new SelectableNodeWithIcon<Object>(LanguageManager.getMessage("Terminologies"), null, true, false, GDLEditorImageUtil.ONTOLOGY_ICON);
	for (String terminologyId : CDSTerminologyService.getDelegate().getSupportedTerminologies()) {
	    SelectableNodeWithIcon<Object> nodeAux = 
		    new SelectableNodeWithIcon<Object>(terminologyId, terminologyId, true, false, GDLEditorImageUtil.ONTOLOGY_ICON);
	    root.add(nodeAux);
	}
	return root;
    }

    public static SelectableNode<Object> getNodeAllTerminologyCodes(String terminologyId, Collection<String> selectedCodes){
	SelectableNodeWithIcon<Object> root = 
		new SelectableNodeWithIcon<Object>(terminologyId, null, true, false, GDLEditorImageUtil.ONTOLOGY_ICON);
	try {
	    List<Node> nodes = CDSTerminologyService.retrieveAll(terminologyId, OpenEHRDataValuesUI.getLanguageCodePhrase());
	    if (nodes!=null){
		Collections.sort(nodes, new Comparator<Node>() {
		    @Override
		    public int compare(Node o1, Node o2) {
			return o1.getValue().getDefiningCode().getCodeString().compareTo(o2.getValue().getDefiningCode().getCodeString());
		    }
		});
		for (Node node : nodes) {
		    root.add(getSelectableNodeTerminologyCodes(node, selectedCodes));
		}
	    }
	} catch (UnsupportedTerminologyException e) {
	    ExceptionHandler.handle(e);
	} catch (UnsupportedLanguageException e) {
	    ExceptionHandler.handle(e);
	}
	return root;
    }

    public static SelectableNode<Object> getSelectableNodeTerminologyCodes(Node node, Collection<String> selectedCodes){
	String code = node.getValue().getDefiningCode().getCodeString();
	SelectableNodeWithIcon<Object> selectableNode = 
		new SelectableNodeWithIcon<Object>(node.getValue().getValue() +" ("+code+")", node.getValue().getDefiningCode(), false, selectedCodes.contains(code), GDLEditorImageUtil.ONTOLOGY_ICON);
	selectableNode.setHierarchySelection(false);
	for (Node node2 : node.getChildren()) {
	    selectableNode.add(getSelectableNodeTerminologyCodes(node2, selectedCodes));
	}
	return selectableNode;
    }
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  1.1 (the 'License'); you may not use this file except in compliance with
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