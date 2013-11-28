package se.cambio.cds.formgen.view.panels;

import com.rits.cloning.Cloner;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.view.swing.DVSwingUtil;
import se.cambio.cds.controller.session.data.ArchetypeReferences;
import se.cambio.openehr.controller.session.data.ArchetypeElements;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.view.panels.DVGenericPanel;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.*;

public class ElementInstanceGroupPanel extends JPanel {

    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private JPanel panel = null;
    private Map<ArchetypeReference, PanelWithButtons> _archetypeReferencePanelMap = null;
    private TermDefinition _termDefinition = null;
    private boolean _viewButtons = true;

    public ElementInstanceGroupPanel(ArchetypeReference archetypeReference, TermDefinition termDefinition, boolean viewButtons){
        _termDefinition = termDefinition;
        _viewButtons = viewButtons;
        panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        this.setLayout(new BorderLayout());
        this.add(panel);
        generateGenericPanelWithButtons(archetypeReference);
    }

    public void addArchetypeReference(ArchetypeReference archetypeReference){
        if (!getArchetypeReferencePanelMap().containsKey(archetypeReference)){
            if (getArchetypeReferencePanelMap().size()==1){
                ArchetypeReference archetypeReference2 = getArchetypeReferencePanelMap().keySet().iterator().next();
                if (isEmpty(archetypeReference2)){
                    removeArchetypeReference(archetypeReference2);
                }
            }
            generateGenericPanelWithButtons(archetypeReference);
        }
    }

    private boolean isEmpty(ArchetypeReference archetypeReference){
        for (ElementInstance elementInstance : archetypeReference.getElementInstancesMap().values()) {
            if (elementInstance.getDataValue()!=null){
                return false;
            }
        }
        return true;
    }

    public JButton createAddButton(ArchetypeReference archetypeReference){
        JButton button = new JButton(OpenEHRImageUtil.ADD_ICON);
        button.setContentAreaFilled(false);
        button.setBorderPainted(false);
        button.setPreferredSize(new Dimension(16,16));
        button.addActionListener(new AddButtonActionListener(archetypeReference));
        return button;
    }

    private class AddButtonActionListener implements ActionListener{
        private ArchetypeReference _archetypeReference = null;
        public AddButtonActionListener(ArchetypeReference archetypeReference){
            _archetypeReference = archetypeReference;
        }
        public void actionPerformed(ActionEvent e) {
            createNewArchetypeReference(_archetypeReference);
        }
    }

    private void createNewArchetypeReference(ArchetypeReference archetypeReference){
        ArchetypeReference clonedAR =  new Cloner().deepClone(archetypeReference);
        for (ElementInstance elementInstanceAux : clonedAR.getElementInstancesMap().values()) {
            elementInstanceAux.setDataValue(null);
            elementInstanceAux.setNullFlavour(GuideUtil.NULL_FLAVOUR_CODE_NO_INFO);
        }
        generateGenericPanelWithButtons(clonedAR);
    }

    public JButton createRemoveButton(ArchetypeReference archetypeReference){
        JButton button = new JButton(OpenEHRImageUtil.DELETE_ICON);
        button.setContentAreaFilled(false);
        button.setPreferredSize(new Dimension(16,16));
        button.setBorderPainted(false);
        button.addActionListener(new RemoveButtonActionListener(archetypeReference));
        return button;
    }

    private class RemoveButtonActionListener implements ActionListener{
        private ArchetypeReference _archetypeReference = null;
        public RemoveButtonActionListener(ArchetypeReference archetypeReference){
            _archetypeReference = archetypeReference;
        }
        public void actionPerformed(ActionEvent e) {
            removeArchetypeReference(_archetypeReference);
            if (getArchetypeReferencePanelMap().isEmpty()){
                createNewArchetypeReference(_archetypeReference);
            }
        }
    }

    private void removeArchetypeReference(ArchetypeReference archetypeReference){
        JPanel panelAux = getArchetypeReferencePanelMap().get(archetypeReference);
        panel.remove(panelAux);
        panel.revalidate();
        panel.repaint();
        getArchetypeReferencePanelMap().remove(archetypeReference);
    }

    private void generateGenericPanelWithButtons(ArchetypeReference ar){
        LinkedHashMap<JLabel, DVGenericPanel> dvGenericPanelsMap = new LinkedHashMap<JLabel, DVGenericPanel>();
        Collection<ArchetypeElementVO> archetypeElementVOs =
                ArchetypeElements.getArchetypeElementsVO(ar.getIdArchetype(), ar.getIdTemplate());
        for (ArchetypeElementVO archetypeElementVO : archetypeElementVOs) {
            ElementInstance elementInstance =
                    ar.getElementInstancesMap().get(archetypeElementVO.getId());
            if (elementInstance!=null){
                JLabel label = DVSwingUtil.createLabelForElement(elementInstance, _termDefinition);
                DVGenericPanel dvGenericPanel = DVSwingUtil.createDVGenericPanel(elementInstance);
                //dvGenericPanel.setDataValue(elementInstance.getDataValue());
                dvGenericPanelsMap.put(label, dvGenericPanel);
            }
        }
        JButton addButton = null;
        JButton removeButton = null;
        if (_viewButtons){
            addButton = createAddButton(ar);
            removeButton = createRemoveButton(ar);
        }
        String arName = ArchetypeReferences.getName(ar);
        PanelWithButtons panelWithButtons =
                new PanelWithButtons(arName, dvGenericPanelsMap, addButton, removeButton);
        panel.add(panelWithButtons);
        panel.revalidate();
        panel.repaint();
        getArchetypeReferencePanelMap().put(ar, panelWithButtons);
    }

    public Collection<ArchetypeReference> getArchetypeReferences(){
        return getArchetypeReferencePanelMap().keySet();
    }

    public Collection<ElementInstance> getElementInstances(){
        Collection<ElementInstance> elementInstances = new ArrayList<ElementInstance>();
        for (ArchetypeReference archetypeReference : getArchetypeReferences()) {
            elementInstances.addAll(archetypeReference.getElementInstancesMap().values());
        }
        return elementInstances;
    }

    private Map<ArchetypeReference, PanelWithButtons> getArchetypeReferencePanelMap(){
        if(_archetypeReferencePanelMap==null){
            _archetypeReferencePanelMap = new HashMap<ArchetypeReference, PanelWithButtons>();
        }
        return _archetypeReferencePanelMap;
    }

    protected void setEditableComponents(boolean editable){
        for (PanelWithButtons panelWithButtons : getArchetypeReferencePanelMap().values()) {
            panelWithButtons.setEditableComponents(editable);
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