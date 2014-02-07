package se.cambio.openehr.view.panels;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.openehr.controller.session.data.CodedTexts;
import se.cambio.openehr.model.archetype.vo.CodedTextVO;
import se.cambio.openehr.util.OpenEHRConst;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.Collection;

public class DVCodedTextPanel extends DVGenericPanel implements DVPanelInterface {

    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private DVTextPanel dvTextPanel;
    private DVComboBoxPanel dvComboBoxPanel;
    private DVHierarchyCodedTextPanel dvHierarchyPanel;

    public DVCodedTextPanel(String idElement, String idTemplate, boolean allowNull, boolean requestFocus){
        super(idElement, idTemplate, allowNull, requestFocus);
        this.setLayout(new BorderLayout());
        this.setBackground(null);
        this.setBorder(BorderFactory.createEmptyBorder(0,0,0,0));
        Collection<CodedTextVO> codedTextsVO =
                CodedTexts.getCodedTextVOs(idTemplate, idElement);
        if (!codedTextsVO.isEmpty()){
            if (OpenEHRConst.LOCAL.equals(codedTextsVO.iterator().next().getTerminology())){
                this.add(getDVComboBoxPanel(), BorderLayout.CENTER);
                for (CodedTextVO codedTextVO : codedTextsVO) {
                    getDVComboBoxPanel().insertOption(
                            codedTextVO.getCode(),
                            codedTextVO.getName(),
                            codedTextVO.getDescription());
                }
            }else{
                this.add(getDvHierarchyPanel(), BorderLayout.CENTER);
            }
        }else{
            this.add(getDVTextPanel(), BorderLayout.CENTER);
        }
    }

    public DVComboBoxPanel getDVComboBoxPanel(){
        if (dvComboBoxPanel==null){
            dvComboBoxPanel = new DVComboBoxPanel(getIdElement(), getIdTemplate(), isAllowsNull(), isRequestFocus()) {
                private static final long serialVersionUID = 1L;
                @Override
                public void setDataValue(DataValue dataValue) {
                    String code = " ";
                    if (dataValue instanceof DvCodedText){
                        code =((DvCodedText)dataValue).getDefiningCode().getCodeString();
                    }
                    getComboBox().setSelectedItem(code);
                }

                @Override
                public DataValue getDataValue() {
                    String code = (String)getComboBox().getSelectedItem();
                    if (!code.trim().isEmpty()){
                        CodedTextVO codedTextVO = CodedTexts.getCodedTextVO(getIdTemplate(), getIdElement(), code);
                        return new DvCodedText(codedTextVO.getName(),codedTextVO.getTerminology(), codedTextVO.getCode());
                    }else{
                        return null;
                    }
                }
            };
        }
        return dvComboBoxPanel;
    }

    public DVHierarchyCodedTextPanel getDvHierarchyPanel(){
        if (dvHierarchyPanel==null){
            dvHierarchyPanel = new DVHierarchyCodedTextPanel(getIdElement(), getIdTemplate(), isAllowsNull(), isRequestFocus());
        }
        return dvHierarchyPanel;
    }

    public DVTextPanel getDVTextPanel(){
        if (dvTextPanel==null){
            dvTextPanel = new DVTextPanel(getIdElement(), getIdTemplate(), isAllowsNull(), isRequestFocus());
        }
        return dvTextPanel;
    }


    public void setMultipleSelection(boolean multipleSelection){
        if (dvHierarchyPanel!=null){
            dvHierarchyPanel.setMultipleSelection(multipleSelection);
        }
    }

    public void setDataValue(DataValue dataValue) {
        if (dvTextPanel!=null){
            dvTextPanel.setDataValue(dataValue);
        }else if (dvComboBoxPanel!=null){
            dvComboBoxPanel.setDataValue(dataValue);
        }else if (dvHierarchyPanel!=null){
            dvHierarchyPanel.setDataValue(dataValue);
        }
    }


    public DataValue getDataValue() {
        if (dvTextPanel!=null){
            return dvTextPanel.getDataValue();
        }else if (dvComboBoxPanel!=null){
            return dvComboBoxPanel.getDataValue();
        }else if (dvHierarchyPanel!=null){
            return dvHierarchyPanel.getDataValue();
        }else{
            return null;
        }
    }

    @Override
    public Collection<JComponent> getJComponents() {
        Collection<JComponent> components = new ArrayList<JComponent>();
        if (dvTextPanel!=null){
            components.addAll(dvTextPanel.getJComponents());
        }else if (dvComboBoxPanel!=null){
            components.addAll(dvComboBoxPanel.getJComponents());
        }else if (dvHierarchyPanel!=null){
            components.addAll(dvHierarchyPanel.getJComponents());
        }
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