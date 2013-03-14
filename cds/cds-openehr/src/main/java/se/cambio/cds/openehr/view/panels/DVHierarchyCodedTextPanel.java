package se.cambio.cds.openehr.view.panels;

import java.awt.FlowLayout;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.SwingUtilities;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;

import se.cambio.cds.openehr.model.codedtext.vo.CodedTextVO;
import se.cambio.cds.openehr.util.ImageUtil;
import se.cambio.cds.openehr.util.OpenEHRLanguageManager;
import se.cambio.cds.openehr.util.OpenEHRDataValuesUI;
import se.cambio.cds.openehr.view.applicationobjects.CodedTexts;
import se.cambio.cds.openehr.view.trees.SelectableNode;
import se.cambio.cds.openehr.view.trees.SelectableNodeWithIcon;
import se.cambio.cds.openehr.view.util.DvList;
import se.cambio.cds.openehr.view.util.NodeConversor;
import se.cambio.cds.openehr.view.util.SelectCodeActionListener;
import se.cambio.cds.util.CDSTerminologyService;
import se.cambio.cds.util.handlers.ExceptionHandler;

public class DVHierarchyCodedTextPanel extends DVGenericPanel{

    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private Collection<String> selectedCodes;
    private JButton codedTextButton = null;
    private SelectableNode<CodedTextVO> rootNode = null;
    private boolean _multipleSelection = false;

    public static String CODE_PROPERTY_CHANGE = "codeChange";

    public DVHierarchyCodedTextPanel(String idElement, String idTemplate, boolean allowNull, boolean requestFocus){
	super(idElement, idTemplate, allowNull, requestFocus);
	this.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 0));
	this.add(getCodedTextButton());
	selectedCodes = new ArrayList<String>();
    }

    protected JButton getCodedTextButton(){
	if (codedTextButton==null){
	    codedTextButton = new JButton(OpenEHRLanguageManager.getMessage("SelectTerm"), ImageUtil.DV_CODED_TEXT_ICON);
	    codedTextButton.addActionListener(new SelectCodeActionListener(this));
	    if (_requestFocus){
		SwingUtilities.invokeLater(new Runnable() {
		    public void run() {
			codedTextButton.requestFocus();
		    }
		});
	    }
	}
	return codedTextButton;
    }

    public SelectableNode<CodedTextVO> getRootNode(){
	if (rootNode==null){
	    rootNode = new SelectableNodeWithIcon<CodedTextVO>(
		    OpenEHRLanguageManager.getMessage("Terms"), null, true, false, ImageUtil.DV_CODED_TEXT_ICON);
	    Collection<CodedTextVO> codedTextsVO = 
		    CodedTexts.getCodedTextVOs(getIdTemplate(), getIdElement());
	    Map<CodedTextVO, SelectableNode<CodedTextVO>> nodeMap = 
		    new HashMap<CodedTextVO, SelectableNode<CodedTextVO>>();
	    for (CodedTextVO codedTextVO : codedTextsVO) {
		addCodedText(rootNode, codedTextVO, nodeMap, _multipleSelection);
	    }
	}
	return rootNode;
    }

    private static void addCodedText(SelectableNode<CodedTextVO> rootNode, CodedTextVO codedTextVO, Map<CodedTextVO, SelectableNode<CodedTextVO>> nodeMap, boolean multipleSelection){
	if (!nodeMap.containsKey(codedTextVO)){
	    SelectableNodeWithIcon<CodedTextVO> node = new SelectableNodeWithIcon<CodedTextVO>(
		    codedTextVO.getName(), codedTextVO, !multipleSelection, false, ImageUtil.DV_CODED_TEXT_ICON, codedTextVO.getDescription());
	    CodedTextVO parentCodedTextVO = codedTextVO.getParentCodedText();
	    if (parentCodedTextVO!=null){
		SelectableNode<CodedTextVO> parentNode = nodeMap.get(parentCodedTextVO);
		if (parentNode!=null){
		    parentNode.add(node);
		}else{
		    addCodedText(rootNode, parentCodedTextVO, nodeMap, multipleSelection);
		}
	    }else{
		rootNode.add(node);
	    }
	    nodeMap.put(codedTextVO, node);
	}
    }

    public void setDataValue(DataValue dataValue) {
	String label = OpenEHRLanguageManager.getMessage("SelectTerm");
	selectedCodes.clear();
	if (dataValue instanceof DvCodedText){
	    String selectedCode = ((DvCodedText)dataValue).getDefiningCode().getCodeString();
	    selectedCodes.add(selectedCode);
	    CodedTextVO codedTextVO = CodedTexts.getCodedTextVO(getIdTemplate(), getIdElement(), selectedCode);
	    if (codedTextVO!=null){
		label = codedTextVO.getName();
	    }else{
		//Asking directly to the terminology service for a description
		//TODO Take it out and make it a generic call
		label = selectedCode;
		String terminologyId = ((DvCodedText)dataValue).getDefiningCode().getTerminologyId().getValue();
		CodePhrase cp = new CodePhrase(terminologyId, selectedCode);
		try {
		    label = CDSTerminologyService.retrieveTerm(cp, OpenEHRDataValuesUI.getLanguageCodePhrase());
		} catch (Exception e) {
		    ExceptionHandler.handle(e);
		}
		if (label==null){
		    try {
			label = CDSTerminologyService.retrieveTerm(cp, OpenEHRDataValuesUI.getDefaultLanguageCodePhrase());
		    } catch (Exception e) {
			ExceptionHandler.handle(e);
		    }    
		}
		if (label==null){
		    label = selectedCode;
		}
	    }
	    if (label.length()>40){
		label = label.substring(0,40)+"...";
	    }
	    getCodedTextButton().setText(label);
	}else if (dataValue instanceof DvList){
	    Collection<CodedTextVO> codedTextVOs = new ArrayList<CodedTextVO>();
	    for (DataValue dv : ((DvList)dataValue).getDataValues()) {
		if (dv instanceof DvCodedText){
		    String selectedCode = ((DvCodedText)dv).getDefiningCode().getCodeString();
		    CodedTextVO codedTextVO = 
			    CodedTexts.getCodedTextVO(getIdTemplate(), getIdElement(), selectedCode);
		    codedTextVOs.add(codedTextVO);
		    NodeConversor.selectObject(getRootNode(), codedTextVO);
		}
	    }
	    addCodedTextCollection(codedTextVOs);
	}
    }

    public void addCodedTextCollection(Collection<CodedTextVO> codedTextVOs){
	StringBuffer label = new StringBuffer();
	String finalLabel = null;
	selectedCodes.clear();
	int i = 0;
	for (CodedTextVO codedTextVO : codedTextVOs) {
	    selectedCodes.add(codedTextVO.getCode());
	    String name = codedTextVO.getName();
	    label.append(name);
	    i++;
	    if (i<codedTextVOs.size()){
		label.append(", ");
	    }
	    if (label.length()>40 && finalLabel==null){
		finalLabel = label.toString().substring(0,40)+"...";
	    }
	}
	if (label.length()>0){
	    if (finalLabel==null){
		finalLabel = label.toString();
	    }
	    getCodedTextButton().setText(finalLabel);
	    getCodedTextButton().setToolTipText(label.toString());
	}
	getCodedTextButton().firePropertyChange(CODE_PROPERTY_CHANGE, 0, 1);
    }

    public DataValue getDataValue(){
	if (selectedCodes.isEmpty()){
	    return null;
	}else{
	    if (_multipleSelection){
		Collection<DataValue> dataValues = new ArrayList<DataValue>();
		for (String selectedCode : selectedCodes) {
		    CodedTextVO codedTextVO = CodedTexts.getCodedTextVO(getIdTemplate(), getIdElement(), selectedCode);
		    dataValues.add(new DvCodedText(codedTextVO.getName(),codedTextVO.getTerminology(), codedTextVO.getCode()));
		}
		return new DvList(dataValues);
	    }else{
		CodedTextVO codedTextVO = CodedTexts.getCodedTextVO(getIdTemplate(), getIdElement(), selectedCodes.iterator().next());
		return new DvCodedText(codedTextVO.getName(),codedTextVO.getTerminology(), codedTextVO.getCode());
	    }
	}
    }

    public void setMultipleSelection(boolean multipleSelection){
	_multipleSelection = multipleSelection;
    }

    public Collection<JComponent> getJComponents() {
	Collection<JComponent> components = new ArrayList<JComponent>();
	components.add(getCodedTextButton());
	return components;
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