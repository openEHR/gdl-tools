package se.cambio.cds.view.swing;

import com.toedter.calendar.JDateChooser;
import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.model.facade.execution.vo.GeneratedElementInstance;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.Domains;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstance;
import se.cambio.cds.controller.session.data.ArchetypeReferences;
import se.cambio.cds.view.swing.applicationobjects.DomainsUI;
import se.cambio.openehr.controller.session.data.ArchetypeElements;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.model.archetype.vo.ClusterVO;
import se.cambio.openehr.util.DisabledComboUI;
import se.cambio.openehr.util.OpenEHRConst;
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

public class DVSwingUtil {
    private static String ITEMS_STR = "/items[";
    private static Border defaultBorder = new LineBorder(Color.BLACK);

    /*
    public static ElementInstance getElementInstance(ArchetypeElementVO archetypeElementVO, boolean last){
	ArchetypeReference ar =
		new ArchetypeReference(
			archetypeElementVO.getIdArchetype(), 
			archetypeElementVO.getIdTemplate(), 
			(last?AggregationFunctions.ID_AGGREGATION_FUNCTION_LAST:null));
	return new ElementInstance(
		archetypeElementVO.getId(), 
		null, ar, null, GuideUtil.NULL_FLAVOUR_VALUE);
    }
     */
    public static ArchetypeReference getEHRArchetypeReference(String idArchetype){
        return new ArchetypeReference(
                Domains.EHR_ID,
                idArchetype,
                null);
    }

    public static ElementInstance getElementInstance(String elementId, ArchetypeReference ar){
        return new ElementInstance(
                elementId,
                null, ar, null, GuideUtil.NULL_FLAVOUR_CODE_NO_INFO);
    }

    public static ElementInstance getEHRElementInstance(String elementId){
        String idArchetype = getIdArchetype(elementId);
        return new ElementInstance(
                elementId,
                null, getEHRArchetypeReference(idArchetype),
                null, GuideUtil.NULL_FLAVOUR_CODE_NO_INFO);
    }

    public static String getIdArchetype(String elementId){
        return elementId.substring(0, elementId.indexOf("/",1));
    }

    public static String getElementIdWithoutSection(String elementId){
        String archetypeId = getIdArchetypeInsideSection(elementId);
        int endArch = elementId.indexOf(archetypeId)+archetypeId.length()+1;
        return archetypeId+elementId.substring(endArch);
    }

    private static String getIdArchetypeInsideSection(String elementId){
        int startArch = elementId.indexOf(ITEMS_STR)+ITEMS_STR.length();
        int endArch = elementId.indexOf("]", startArch);
        return elementId.substring(startArch, endArch);
    }

    public static ArchetypeElementVO getArchetypeElementVO(ElementInstance elementInstance){
        String idTemplate = null;
        if (elementInstance.getArchetypeReference()!=null){
            idTemplate = elementInstance.getArchetypeReference().getIdTemplate();
        }
        ArchetypeElementVO ae = ArchetypeElements.getArchetypeElement(idTemplate, elementInstance.getId());
        if (ae==null){
            Logger.getLogger(DVSwingUtil.class).warn("Archetype element '"+elementInstance.getId()+"' not found.");
        }
        return ae;
    }

    public static JLabel createLabelForElement(ArchetypeElementVO archetypeElementVO){
        JLabel label = null;
        String name = archetypeElementVO.getName();
        String tooltip = name;

        label = new JLabel(name);
        label.setToolTipText(tooltip);

        label.setToolTipText(ArchetypeReferences.getHTMLTooltip(archetypeElementVO, null));
        Icon dvIcon = OpenEHRDataValuesUI.getIcon(archetypeElementVO.getRMType());
        label.setIcon(dvIcon);
        return label;
    }

    public static JLabel createLabelForElement(ElementInstance elementInstance, TermDefinition termDefinition){
        ArchetypeElementVO archetypeElement =
                ArchetypeElements.getArchetypeElement(
                        elementInstance.getArchetypeReference().getIdTemplate(),
                        elementInstance.getId());
        JLabel label = null;
        String name = archetypeElement.getName();
        if (elementInstance instanceof GeneratedElementInstance){
            GeneratedElementInstance elementInstancesWithGTCode = (GeneratedElementInstance) elementInstance;
            Term term = null;
            if (termDefinition!=null){
                term = termDefinition.getTerms().get(elementInstancesWithGTCode.getGtCode());
            }
            if (term!=null){
                name =  term.getText();
            }else{
                Logger.getLogger(DVSwingUtil.class).warn("Term translation not found for '"+elementInstance.getId()+"'");
            }
        }

	/*if(_formGenerator.isLinkedInput(elementInstance)){
	    label = new JLinkLabel();
	    label.setText(name);
	    label.setToolTipText(tooltip);
	    ((JLinkLabel)label).addActionListener(_formGenerator.createLinkedFormGeneratorActionListener(elementInstance));
	}else{*/
        label = new JLabel(name);
        //}
        label.setToolTipText(
                ArchetypeReferences.getHTMLTooltip(
                        archetypeElement,
                        elementInstance.getArchetypeReference()));
        Icon dvIcon = OpenEHRDataValuesUI.getIcon(archetypeElement.getRMType());
        Icon domainIcon =
                DomainsUI.getGroupIconFromArchetypeReference(elementInstance.getArchetypeReference());
        label.setIcon(new MultipleIcon(new Icon[]{domainIcon, dvIcon}));
        return label;
    }

    public static ClusterVO getSection(ArchetypeElementVO archetypeElementVO, int level){
        int sectionCount = 0;
        for (ClusterVO clusterVO : ArchetypeElements.getClusters(archetypeElementVO)) {
            if (clusterVO.getRMType().equals(OpenEHRConst.SECTION) && sectionCount==level){
                return clusterVO;
            }else{
                sectionCount++;
            }
        }
        return null;
    }

    public static DVGenericPanel getDVGenericForElement(ArchetypeElementVO archetypeElement){
        String rmType = archetypeElement.getRMType();
        DVGenericPanel dvGenericPanel =
                DVPanelFactory.createDVPanel(
                        archetypeElement.getId(),
                        archetypeElement.getIdTemplate(),
                        rmType, true, false, false);
        return dvGenericPanel;
    }

    public static DVGenericPanel createDVGenericPanel(ElementInstance elementInstance){
        DVGenericPanel dvGenericPanel = getDVGenericForElement(elementInstance);
        for (JComponent jComponent : dvGenericPanel.getJComponents()) {
            if (jComponent instanceof JDateChooser){
                ((JDateChooser)jComponent).getDateEditor().getUiComponent().addFocusListener(new DVGenericComponentFocusAdapter(dvGenericPanel, elementInstance));
            }else{
                jComponent.addFocusListener(new DVGenericComponentFocusAdapter(dvGenericPanel, elementInstance));
            }
            if (Domains.CDS_ID.equals(elementInstance.getArchetypeReference().getIdDomain()) ||
                    elementInstance instanceof PredicateGeneratedElementInstance){
                DVSwingUtil.disable(jComponent);
            }
        }
        return dvGenericPanel;
    }

    public static DVGenericPanel getDVGenericForElement(ElementInstance elementInstance){
        String idTemplate = elementInstance.getArchetypeReference().getIdTemplate();
        String idElement = elementInstance.getId();
        ArchetypeElementVO archetypeElement =
                ArchetypeElements.getArchetypeElement(
                        idTemplate,
                        idElement);
        String rmType = archetypeElement.getRMType();
        DVGenericPanel dvGenericPanel = DVPanelFactory.createDVPanel(idElement, idTemplate, rmType, true, true, false);
        if (dvGenericPanel!=null){
            dvGenericPanel.setDataValue(elementInstance.getDataValue());
        }
        return dvGenericPanel;
    }


    public static class DVGenericComponentFocusAdapter extends FocusAdapter{
        private DVGenericPanel dvGenericPanel = null;
        private ElementInstance elementInstance = null;

        public DVGenericComponentFocusAdapter(DVGenericPanel dvGenericPanel, ElementInstance elementInstance){
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
            Logger.getLogger(DVSwingUtil.class).warn("Element '"+jComponent.getClass().getName()+"' has not been disabled");
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
            Logger.getLogger(DVSwingUtil.class).warn("Element '"+jComponent.getClass().getName()+"' has not been disabled");
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