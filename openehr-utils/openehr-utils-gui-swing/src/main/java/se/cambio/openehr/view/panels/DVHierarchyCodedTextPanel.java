package se.cambio.openehr.view.panels;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.controller.session.data.CodedTexts;
import se.cambio.cm.model.archetype.vo.CodedTextVO;
import se.cambio.openehr.util.*;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.view.util.SelectCodeActionListener;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.Collection;

public class DVHierarchyCodedTextPanel extends DVGenericPanel implements TerminologyCodesManager{

    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private Collection<DvCodedText> selectedDvCodeTexts;
    private JButton codedTextButton = null;

    public static String CODE_PROPERTY_CHANGE = "codeChange";

    public DVHierarchyCodedTextPanel(String idElement, String idTemplate, boolean allowNull, boolean requestFocus){
        super(idElement, idTemplate, allowNull, requestFocus);
        this.setLayout(new BorderLayout());
        this.add(getCodedTextButton(), BorderLayout.CENTER);
        selectedDvCodeTexts = new ArrayList<DvCodedText>();
    }

    protected JButton getCodedTextButton(){
        if (codedTextButton==null){
            codedTextButton = new JButton(OpenEHRLanguageManager.getMessage("SelectTerm"), OpenEHRImageUtil.DV_CODED_TEXT_ICON);
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

    public void setDataValue(DataValue dataValue) {
        String label;
        selectedDvCodeTexts.clear();
        if (dataValue instanceof DvCodedText){
            DvCodedText selectedCodedText = (DvCodedText)dataValue;
            selectedDvCodeTexts.add(selectedCodedText);
            CodedTextVO codedTextVO = getCodedTexts().getCodedTextVO(getIdTemplate(), getIdElement(), selectedCodedText.getCode());
            if (codedTextVO!=null){
                label = getCodedTexts().getText(codedTextVO, UserConfigurationManager.instance().getLanguage());
            }else{
                //Asking directly to the terminology service for a description
                //TODO Take it out and make it a generic call
                label = selectedCodedText.getCode();
                String terminologyId = ((DvCodedText)dataValue).getDefiningCode().getTerminologyId().getValue();
                CodePhrase cp = new CodePhrase(terminologyId, selectedCodedText.getCode());
                try {
                    label = OpenEHRSessionManager.getTerminologyFacadeDelegate().retrieveTerm(cp, OpenEHRDataValuesUI.getLanguageCodePhrase());
                } catch (Exception e) {
                    ExceptionHandler.handle(e);
                }
                if (label==null){
                    try {
                        label = OpenEHRSessionManager.getTerminologyFacadeDelegate().retrieveTerm(cp, OpenEHRDataValuesUI.getDefaultLanguageCodePhrase());
                    } catch (Exception e) {
                        ExceptionHandler.handle(e);
                    }
                }
                if (label==null){
                    label = selectedCodedText.getCode();
                }
            }
            if (label.length()>40){
                label = label.substring(0,40)+"...";
            }
            getCodedTextButton().setText(label);
        }
    }

    public DataValue getDataValue(){
        if (selectedDvCodeTexts.isEmpty()){
            return null;
        }else{
            return selectedDvCodeTexts.iterator().next();
        }
    }

    private CodedTexts getCodedTexts(){
        return ArchetypeManager.getInstance().getCodedTexts();
    }

    public Collection<JComponent> getJComponents() {
        Collection<JComponent> components = new ArrayList<JComponent>();
        components.add(getCodedTextButton());
        return components;
    }

    public String getTerminologyId(){
        Collection<CodedTextVO> codedTextVOs = getCodedTexts().getCodedTextVOs(getIdTemplate(), getIdElement());
        return codedTextVOs.iterator().next().getTerminology();
    }

    @Override
    public void update() {
        //Generated
    }

    @Override
    public void setSelectedTerminologyCodes(Collection<String> terminologyCodes) {
        StringBuffer label = new StringBuffer();
        String finalLabel = null;
        selectedDvCodeTexts.clear();
        String prefix = "";
        String terminologyId = getTerminologyId();
        CodePhrase langCodePhrase = OpenEHRDataValuesUI.getLanguageCodePhrase();
        for (String code : terminologyCodes) {
            CodePhrase codePhrase = new CodePhrase(terminologyId, code);
            String name = null;
            try {
                name = OpenEHRSessionManager.getTerminologyFacadeDelegate().retrieveTerm(codePhrase, langCodePhrase);
            } catch (InternalErrorException e) {
                ExceptionHandler.handle(e);
                name = code;
            }
            selectedDvCodeTexts.add(new DvCodedText(name, codePhrase));
            label.append(prefix);
            label.append(name);
            prefix = ", ";
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

    public Collection<String> getSelectedCodes() {
        Collection<String> selectedCodes = new ArrayList<String>();
        for(DvCodedText dvCodedText: selectedDvCodeTexts){
            selectedCodes.add(dvCodedText.getCode());
        }
        return selectedCodes;
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