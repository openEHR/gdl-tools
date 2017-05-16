package se.cambio.cds.view.swing;

import com.toedter.calendar.JDateChooser;
import org.openehr.rm.datatypes.basic.DataValue;
import org.slf4j.LoggerFactory;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.controller.session.data.ArchetypeReferencesManager;
import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.model.facade.execution.vo.GeneratedElementInstance;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstance;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.Domains;
import se.cambio.cds.view.swing.applicationobjects.DomainsUI;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.DisabledComboUI;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.OpenEHRDataValuesUI;
import se.cambio.openehr.view.panels.DVGenericPanel;
import se.cambio.openehr.view.util.DVPanelFactory;
import se.cambio.openehr.view.util.MultipleIcon;
import se.cambio.openehr.view.util.SelectCodeActionListener;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.LineBorder;
import javax.swing.plaf.ComboBoxUI;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;

public class DvSwingManager {
    private static Border defaultBorder = new LineBorder(Color.BLACK);
    private ArchetypeManager archetypeManager;
    private ArchetypeReferencesManager archetypeReferencesManager;
    private DVPanelFactory dvPanelFactory;

    public DvSwingManager(ArchetypeManager archetypeManager,
                          ArchetypeReferencesManager archetypeReferencesManager,
                          DVPanelFactory dvPanelFactory) {
        this.archetypeManager = archetypeManager;
        this.archetypeReferencesManager = archetypeReferencesManager;
        this.dvPanelFactory = dvPanelFactory;
    }

    public JLabel createLabelForElement(ElementInstance elementInstance, TermDefinition termDefinition) {
        ArchetypeElementVO archetypeElement =
                archetypeManager.getArchetypeElements().getArchetypeElement(
                        elementInstance.getArchetypeReference().getIdTemplate(),
                        elementInstance.getId());
        JLabel label;
        String name = archetypeManager.getArchetypeElements().getText(archetypeElement, archetypeManager.getUserConfigurationManager().getLanguage());

        if (elementInstance instanceof GeneratedElementInstance){
            GeneratedElementInstance elementInstancesWithGTCode = (GeneratedElementInstance) elementInstance;
            //Labels are only going to be shown if there is only one term definition
            if (elementInstancesWithGTCode.getRuleReferences().size()<2){
                Term term = null;
                if (termDefinition!=null){
                    String gtCode = null;
                    if (!elementInstancesWithGTCode.getRuleReferences().isEmpty()){
                        gtCode = elementInstancesWithGTCode.getRuleReferences().iterator().next().getGtCode();
                    }
                    term = termDefinition.getTerms().get(gtCode);
                }
                if (term!=null){
                    name =  term.getText();
                }else{
                    LoggerFactory.getLogger(DvSwingManager.class).warn("Term translation not found for '"+elementInstance.getId()+"'");
                }
            }
        }
        label = new JLabel(name);
        label.setToolTipText(
                archetypeReferencesManager.getHTMLTooltip(
                        archetypeElement,
                        elementInstance.getArchetypeReference()));
        Icon dvIcon = OpenEHRDataValuesUI.getIcon(archetypeElement.getRMType());
        Icon domainIcon =
                DomainsUI.getGroupIconFromArchetypeReference(elementInstance.getArchetypeReference());
        label.setIcon(new MultipleIcon(new Icon[]{domainIcon, dvIcon}));
        return label;
    }

    public DVGenericPanel createDVGenericPanel(ElementInstance elementInstance) {
        if (!Domains.CDS_ID.equals(elementInstance.getArchetypeReference().getIdDomain()) &&
                elementInstance instanceof PredicateGeneratedElementInstance){
            //Remove predicates for ehr data
            elementInstance = new ElementInstance(
                    elementInstance.getId(),
                    null,
                    elementInstance.getArchetypeReference(),
                    null,
                    OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        }
        DVGenericPanel dvGenericPanel = getDVGenericForElement(elementInstance);
        for (JComponent jComponent : dvGenericPanel.getJComponents()) {
            if (jComponent instanceof JDateChooser){
                ((JDateChooser)jComponent).getDateEditor().getUiComponent().addFocusListener(new DVGenericComponentFocusAdapter(dvGenericPanel, elementInstance));
            }else{
                jComponent.addFocusListener(new DVGenericComponentFocusAdapter(dvGenericPanel, elementInstance));
            }
            if (Domains.CDS_ID.equals(elementInstance.getArchetypeReference().getIdDomain()) ||
                    elementInstance instanceof PredicateGeneratedElementInstance){
                DvSwingManager.disable(jComponent);
            }
        }
        return dvGenericPanel;
    }

    private DVGenericPanel getDVGenericForElement(ElementInstance elementInstance) {
        String idTemplate = elementInstance.getArchetypeReference().getIdTemplate();
        String idElement = elementInstance.getId();
        ArchetypeElementVO archetypeElement =
                archetypeManager.getArchetypeElements().getArchetypeElement(
                        idTemplate,
                        idElement);
        String rmType = archetypeElement.getRMType();
        DVGenericPanel dvGenericPanel = dvPanelFactory.createDVPanel(idElement, idTemplate, rmType, true, true, false);
        if (dvGenericPanel != null){
            dvGenericPanel.setDataValue(elementInstance.getDataValue());
        }
        return dvGenericPanel;
    }


    public static class DVGenericComponentFocusAdapter extends FocusAdapter{
        private DVGenericPanel dvGenericPanel = null;
        private ElementInstance elementInstance = null;

        DVGenericComponentFocusAdapter(DVGenericPanel dvGenericPanel, ElementInstance elementInstance){
            this.dvGenericPanel = dvGenericPanel;
            this.elementInstance = elementInstance;
        }

        public void focusLost(FocusEvent ev) {
            DataValue dataValue = null;
            if (dvGenericPanel.isValidDV()){
                dataValue = dvGenericPanel.getDataValue();
            }
            elementInstance.setDataValue(dataValue);
            elementInstance.setNullFlavour(dataValue!=null?null:GuideUtil.NULL_FLAVOUR_CODE_NO_INFO);
        }
    }

    public static void disable(JComponent jComponent){
        if (jComponent instanceof JTextField){
            JTextField textField = ((JTextField)jComponent);
            textField.setEditable(false);
            //TODO Improve
            defaultBorder = textField.getBorder();
            textField.setBorder(null);
        }else if (jComponent instanceof JComboBox){
            disable((JComboBox)jComponent);
        }else if (jComponent instanceof JButton){
            JButton button = ((JButton)jComponent);
            button.setContentAreaFilled(false);
            button.setBorderPainted(false);
            setEnableActionListeners(false, button);
        }else if (jComponent instanceof JDateChooser){
            JDateChooser jDateChooser = (JDateChooser)jComponent;
            disable(jDateChooser.getDateEditor().getUiComponent());
            jDateChooser.getCalendarButton().setVisible(false);
        }else{
            LoggerFactory.getLogger(DvSwingManager.class).warn("Element '"+jComponent.getClass().getName()+"' has not been disabled");
        }
    }

    private static void setEnableActionListeners(boolean enable, JButton button){
        for (ActionListener actionListener : button.getActionListeners()) {
            if(actionListener instanceof SelectCodeActionListener){
                ((SelectCodeActionListener)actionListener).setEnable(enable);
            }
        }
    }

    public static void enable(JComponent jComponent){
        if (jComponent instanceof JTextField){
            JTextField textField = ((JTextField)jComponent);
            textField.setEditable(true);
            textField.setBorder(defaultBorder);
        }else if (jComponent instanceof JComboBox){
            enable((JComboBox)jComponent);
        }else if (jComponent instanceof JButton){
            JButton button = ((JButton)jComponent);
            button.setContentAreaFilled(true);
            button.setBorderPainted(true);
            setEnableActionListeners(true, button);
        }else if (jComponent instanceof JDateChooser){
            JDateChooser jDateChooser = (JDateChooser)jComponent;
            enable(jDateChooser.getDateEditor().getUiComponent());
            jDateChooser.getCalendarButton().setVisible(true);
        }else{
            LoggerFactory.getLogger(DvSwingManager.class).warn("Element '"+jComponent.getClass().getName()+"' has not been disabled");
        }
    }

    private static void disable(JComboBox comboBox){
        comboBox.setFocusable(false);
        comboBox.setUI(new DisabledComboUI());
        comboBox.setFont(comboBox.getFont().deriveFont(Font.PLAIN));
        ComboBoxEditor editor = comboBox.getEditor();
        if (editor!=null && editor.getEditorComponent() instanceof JTextComponent){
            ((JTextComponent)editor.getEditorComponent()).setEditable(false);
        }
    }

    private static void enable(JComboBox component){
        component.setFocusable(true);
        component.setUI((ComboBoxUI)UIManager.getUI(component));
        ComboBoxEditor editor = component.getEditor();
        if (editor!=null && editor.getEditorComponent() instanceof JTextComponent){
            ((JTextComponent)editor.getEditorComponent()).setEditable(true);
        }
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