package se.cambio.cds.formgen.view.panels;

import com.rits.cloning.Cloner;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.controller.session.data.ArchetypeReferencesManager;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.view.swing.DvSwingManager;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.view.panels.DVGenericPanel;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.*;

class ElementInstanceGroupPanel extends JPanel {

    private static final long serialVersionUID = 1L;
    private JPanel panel = null;
    private Map<ArchetypeReference, PanelWithButtons> archetypeReferencePanelMap = null;
    private TermDefinition _termDefinition = null;
    private boolean _viewButtons = true;
    private final ArchetypeManager archetypeManager;
    private DvSwingManager dvSwingManager;

    ElementInstanceGroupPanel(
            ArchetypeReference archetypeReference,
            TermDefinition termDefinition, boolean viewButtons,
            ArchetypeManager archetypeManager,
            DvSwingManager dvSwingManager) {
        _termDefinition = termDefinition;
        _viewButtons = viewButtons;
        this.archetypeManager = archetypeManager;
        this.dvSwingManager = dvSwingManager;
        panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        this.setLayout(new BorderLayout());
        this.add(panel);
        generateGenericPanelWithButtons(archetypeReference);
    }

    private JButton createAddButton(ArchetypeReference archetypeReference) {
        JButton button = new JButton(OpenEHRImageUtil.ADD_ICON);
        button.setContentAreaFilled(false);
        button.setBorderPainted(false);
        button.setPreferredSize(new Dimension(16, 16));
        button.addActionListener(new AddButtonActionListener(archetypeReference));
        return button;
    }

    private class AddButtonActionListener implements ActionListener {
        private ArchetypeReference _archetypeReference = null;

        AddButtonActionListener(ArchetypeReference archetypeReference) {
            _archetypeReference = archetypeReference;
        }

        public void actionPerformed(ActionEvent e) {
            createNewArchetypeReference(_archetypeReference);
        }
    }

    private void createNewArchetypeReference(ArchetypeReference archetypeReference) {
        ArchetypeReference clonedAR = new Cloner().deepClone(archetypeReference);
        for (ElementInstance elementInstanceAux : clonedAR.getElementInstancesMap().values()) {
            elementInstanceAux.setDataValue(null);
            elementInstanceAux.setNullFlavour(GuideUtil.NULL_FLAVOUR_CODE_NO_INFO);
        }
        generateGenericPanelWithButtons(clonedAR);
    }

    private JButton createRemoveButton(ArchetypeReference archetypeReference) {
        JButton button = new JButton(OpenEHRImageUtil.DELETE_ICON);
        button.setContentAreaFilled(false);
        button.setPreferredSize(new Dimension(16, 16));
        button.setBorderPainted(false);
        button.addActionListener(new RemoveButtonActionListener(archetypeReference));
        return button;
    }

    private class RemoveButtonActionListener implements ActionListener {
        private ArchetypeReference _archetypeReference = null;

        RemoveButtonActionListener(ArchetypeReference archetypeReference) {
            _archetypeReference = archetypeReference;
        }

        public void actionPerformed(ActionEvent e) {
            removeArchetypeReference(_archetypeReference);
            if (getArchetypeReferencePanelMap().isEmpty()) {
                createNewArchetypeReference(_archetypeReference);
            }
        }
    }

    private void removeArchetypeReference(ArchetypeReference archetypeReference) {
        JPanel panelAux = getArchetypeReferencePanelMap().get(archetypeReference);
        panel.remove(panelAux);
        panel.revalidate();
        panel.repaint();
        getArchetypeReferencePanelMap().remove(archetypeReference);
    }

    private void generateGenericPanelWithButtons(ArchetypeReference ar) {
        LinkedHashMap<JLabel, DVGenericPanel> dvGenericPanelsMap = new LinkedHashMap<>();
        Collection<ArchetypeElementVO> archetypeElementVOs =
                archetypeManager.getArchetypeElements().getArchetypeElementsVO(ar.getIdArchetype(), ar.getIdTemplate());
        for (ArchetypeElementVO archetypeElementVO : archetypeElementVOs) {
            ElementInstance elementInstance =
                    ar.getElementInstancesMap().get(archetypeElementVO.getId());
            if (elementInstance != null) {
                JLabel label = dvSwingManager.createLabelForElement(elementInstance, _termDefinition);
                DVGenericPanel dvGenericPanel = dvSwingManager.createDVGenericPanel(elementInstance);
                dvGenericPanelsMap.put(label, dvGenericPanel);
            }
        }
        JButton addButton = null;
        JButton removeButton = null;
        if (_viewButtons) {
            addButton = createAddButton(ar);
            removeButton = createRemoveButton(ar);
        }
        String arName = ArchetypeReferencesManager.getName(ar);
        PanelWithButtons panelWithButtons =
                new PanelWithButtons(arName, dvGenericPanelsMap, addButton, removeButton);
        panel.add(panelWithButtons);
        panel.revalidate();
        panel.repaint();
        getArchetypeReferencePanelMap().put(ar, panelWithButtons);
    }

    private Collection<ArchetypeReference> getArchetypeReferences() {
        return getArchetypeReferencePanelMap().keySet();
    }

    Collection<ElementInstance> getElementInstances() {
        Collection<ElementInstance> elementInstances = new ArrayList<>();
        for (ArchetypeReference archetypeReference : getArchetypeReferences()) {
            elementInstances.addAll(archetypeReference.getElementInstancesMap().values());
        }
        return elementInstances;
    }

    private Map<ArchetypeReference, PanelWithButtons> getArchetypeReferencePanelMap() {
        if (archetypeReferencePanelMap == null) {
            archetypeReferencePanelMap = new HashMap<>();
        }
        return archetypeReferencePanelMap;
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