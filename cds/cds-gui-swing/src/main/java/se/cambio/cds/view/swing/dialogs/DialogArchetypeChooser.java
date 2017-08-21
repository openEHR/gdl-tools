package se.cambio.cds.view.swing.dialogs;

import se.cambio.cds.util.Domains;
import se.cambio.cds.view.swing.applicationobjects.DomainsUI;
import se.cambio.cm.model.util.CMElement;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.view.panels.SelectionPanel;
import se.cambio.openehr.view.trees.SelectableNode;
import se.cambio.openehr.view.trees.SelectableNodeBuilder;
import se.cambio.openehr.view.util.ImportManager;
import se.cambio.openehr.view.util.NodeConversor;
import se.cambio.openehr.view.util.ScreenUtil;
import se.cambio.openehr.view.util.WindowManager;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.Collection;


public class DialogArchetypeChooser extends JDialog {

    private static final long serialVersionUID = 1L;
    private ImportManager importManager;
    private ArchetypeManager archetypeManager;
    private JButton acceptButton;
    private JButton cancelButton;
    private JPanel jPanel;
    private JPanel bottomPanel;
    private boolean _answer = false;
    private AcceptChangesAction acceptChangesAction;
    private CancelChangesAction cancelChangesAction;
    private SelectionPanel archetypeSelectionPanel;
    private SelectionPanel templateSelectionPanel;
    private SelectableNode<String> archetypeNode = null;
    private SelectableNode<String> templateNode = null;
    private JComboBox<String> domainComboBox;
    private JTabbedPane tabbedPane;

    private String ANY_DOMAIN = "*";
    private JLabel domainLabel;
    private WindowManager windowManager;

    public DialogArchetypeChooser(Window owner, WindowManager windowManager,
                                  String archetypeId, String domainId,
                                  boolean selectTemplates, boolean onlyShowCDS,
                                  ImportManager importManager, ArchetypeManager archetypeManager) {
        super(
                owner,
                OpenEHRLanguageManager.getMessage("Archetypes") + "/" + OpenEHRLanguageManager.getMessage("Templates"),
                ModalityType.APPLICATION_MODAL);
        this.windowManager = windowManager;
        this.importManager = importManager;
        this.archetypeManager = archetypeManager;
        if (onlyShowCDS) {
            getDomainSelector().setSelectedItem(Domains.CDS_ID);
            getDomainSelector().setEnabled(false);
        } else if (archetypeId != null) {
            if (domainId == null) {
                domainId = ANY_DOMAIN;
            }
            if (!Domains.EHR_ID.equals(domainId)) {
                getDomainSelector().setSelectedItem(domainId);
            }
        }
        init(new Dimension(500, 500), selectTemplates);
    }

    private void init(Dimension size, boolean selectTemplates) {
        this.setSize(size);
        ScreenUtil.centerComponentOnScreen(this, this.getOwner());
        this.setResizable(true);
        this.addWindowListener(getCancelChangesAction());
        this.setContentPane(getJPanel());
    /* Enter KeyStroke */
        KeyStroke enter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, true);
        getJPanel().registerKeyboardAction(getAcceptChangesAction(), enter, JComponent.WHEN_IN_FOCUSED_WINDOW);
        KeyStroke esc = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, true);
        getJPanel().registerKeyboardAction(getCancelChangesAction(), esc, JComponent.WHEN_IN_FOCUSED_WINDOW);
        if (selectTemplates) {
            getArchetypeTemplateTabbedPane().setSelectedIndex(1);
        } else {
            getArchetypeTemplateTabbedPane().setSelectedIndex(0);
        }
    }

    private JPanel getJPanel() {
        if (jPanel == null) {
            jPanel = new JPanel(new BorderLayout());
            JPanel aux = new JPanel(new FlowLayout(FlowLayout.LEFT));
            aux.add(getDomainLabel());
            aux.add(getDomainSelector());
            jPanel.add(aux, BorderLayout.NORTH);
            jPanel.add(getArchetypeTemplateTabbedPane(), BorderLayout.CENTER);
            JPanel panelAux = new JPanel(new BorderLayout());
            jPanel.add(panelAux, BorderLayout.SOUTH);
            panelAux.add(getBottomPanel(), BorderLayout.SOUTH);
        }
        return jPanel;
    }

    private JLabel getDomainLabel() {
        if (domainLabel == null) {
            domainLabel = new JLabel(OpenEHRLanguageManager.getMessage("Domain") + ":");
        }
        return domainLabel;
    }

    private JComboBox getDomainSelector() {
        if (domainComboBox == null) {
            domainComboBox = new JComboBox<>();
            domainComboBox.addItem(ANY_DOMAIN);
            domainComboBox.addItem(Domains.EHR_ID);
            domainComboBox.addItem(Domains.CDS_ID);
            domainComboBox.setRenderer(new DomainComboBoxRenderer<>());
            domainComboBox.setSelectedItem(Domains.EHR_ID);
        }
        return domainComboBox;
    }

    private class DomainComboBoxRenderer<E> extends JLabel implements ListCellRenderer<E> {

        private static final long serialVersionUID = 1L;

        @Override
        public Component getListCellRendererComponent(JList list, Object value,
                                                      int index, boolean isSelected, boolean cellHasFocus) {
            if (isSelected) {
                setBackground(list.getSelectionBackground());
                setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
            }
            if (value instanceof String) {
                String idDomain = (String) value;
                if (idDomain.equals(ANY_DOMAIN)) {
                    idDomain = null;
                }
                setText(DomainsUI.getName(idDomain));
                setToolTipText(DomainsUI.getDescription(idDomain));
                setIcon(DomainsUI.getIcon(idDomain));
            } else {
                setText(null);
                setToolTipText(null);
                setIcon(null);
            }
            return this;
        }
    }

    private JTabbedPane getArchetypeTemplateTabbedPane() {
        if (tabbedPane == null) {
            tabbedPane = new JTabbedPane();
            refreshArchetypeSelectionPanel();
            refreshTemplateSelectionPanel();
        }
        return tabbedPane;
    }

    private JPanel getArchetypeSelectionPanel() {
        if (archetypeSelectionPanel == null) {
            archetypeSelectionPanel = new SelectionPanel(windowManager, getArchetypeNode());
            archetypeSelectionPanel.getJTree().expand(getArchetypeNode());
            archetypeSelectionPanel.getJTree().addExtraMouseListener(new DoubleClickMouseListener());
            JButton addArchetypeButton = new JButton(OpenEHRLanguageManager.getMessage("Import"));
            addArchetypeButton.setIcon(OpenEHRImageUtil.FOLDER_ICON);
            addArchetypeButton.addActionListener(new ImportArchetypeActionListener(this));
            archetypeSelectionPanel.getFilterPanel().add(addArchetypeButton);
        }
        return archetypeSelectionPanel;
    }

    private class ImportArchetypeActionListener implements ActionListener {
        private DialogArchetypeChooser _dialog = null;

        ImportArchetypeActionListener(DialogArchetypeChooser dialog) {
            _dialog = dialog;
        }

        public void actionPerformed(ActionEvent ev) {
            importManager.showImportArchetypeDialogAndAddToRepo(_dialog, null);
            refreshArchetypeSelectionPanel();
        }
    }


    private void refreshArchetypeSelectionPanel() {
        archetypeSelectionPanel = null;
        archetypeNode = null;
        getArchetypeSelectionPanel();
        if (getArchetypeTemplateTabbedPane().getTabCount() > 0) {
            getArchetypeTemplateTabbedPane().removeTabAt(0);
        }
        getArchetypeTemplateTabbedPane().insertTab(
                OpenEHRLanguageManager.getMessage("Archetypes"),
                Archetypes.ICON, getArchetypeSelectionPanel(),
                OpenEHRLanguageManager.getMessage("Archetypes"), 0);
        getArchetypeTemplateTabbedPane().setSelectedIndex(0);
    }

    private JPanel getTemplateSelectionPanel() {
        if (templateSelectionPanel == null) {
            templateSelectionPanel = new SelectionPanel(windowManager, getTemplateNode());
            templateSelectionPanel.getJTree().expand(getTemplateNode());
            templateSelectionPanel.getJTree().addExtraMouseListener(new DoubleClickMouseListener());
            JButton addTemplateButton = new JButton(OpenEHRLanguageManager.getMessage("Import"));
            addTemplateButton.setIcon(OpenEHRImageUtil.FOLDER_ICON);
            addTemplateButton.addActionListener(new ImportTemplateActionListener(this));
            templateSelectionPanel.getFilterPanel().add(addTemplateButton);
        }
        return templateSelectionPanel;
    }

    private class ImportTemplateActionListener implements ActionListener {
        private DialogArchetypeChooser _dialog = null;

        ImportTemplateActionListener(DialogArchetypeChooser dialog) {
            _dialog = dialog;
        }

        public void actionPerformed(ActionEvent ev) {
            importManager.showImportTemplateDialog(_dialog, null);
            refreshTemplateSelectionPanel();
        }
    }

    private void refreshTemplateSelectionPanel() {
        templateSelectionPanel = null;
        templateNode = null;
        getTemplateSelectionPanel();
        if (getArchetypeTemplateTabbedPane().getTabCount() > 1) {
            getArchetypeTemplateTabbedPane().removeTabAt(1);
        }
        getArchetypeTemplateTabbedPane().insertTab(
                OpenEHRLanguageManager.getMessage("Templates"),
                Templates.ICON, getTemplateSelectionPanel(),
                OpenEHRLanguageManager.getMessage("Templates"), 1);
        getArchetypeTemplateTabbedPane().setSelectedIndex(1);
    }

    class DoubleClickMouseListener extends MouseAdapter {
        public void mouseClicked(MouseEvent ev) {
            if (ev.getClickCount() > 1) {
                CMElement cmElement = getSelectedCMElement();
                if (cmElement != null) {
                    accept();
                }
            }
        }
    }

    private void accept() {
        _answer = true;
        setVisible(false);
    }

    private void exit() {
        _answer = false;
        setVisible(false);
    }

    private JPanel getBottomPanel() {
        if (bottomPanel == null) {
            bottomPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
            bottomPanel.add(getAcceptButton());
            bottomPanel.add(getCancelButton());
        }
        return bottomPanel;
    }

    private JButton getAcceptButton() {
        if (acceptButton == null) {
            acceptButton = new JButton();
            acceptButton.setText(OpenEHRLanguageManager.getMessage("Accept"));
            acceptButton.setIcon(OpenEHRImageUtil.ACCEPT_ICON);
            acceptButton.setEnabled(true);
            acceptButton.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
            acceptButton.addActionListener(getAcceptChangesAction());
        }
        return acceptButton;
    }

    private JButton getCancelButton() {
        if (cancelButton == null) {
            cancelButton = new JButton();
            cancelButton.setText(OpenEHRLanguageManager.getMessage("Cancel"));
            cancelButton.setIcon(OpenEHRImageUtil.CANCEL_ICON);
            cancelButton.setEnabled(true);
            cancelButton.addActionListener(getCancelChangesAction());
        }
        return cancelButton;
    }

    private AcceptChangesAction getAcceptChangesAction() {
        if (acceptChangesAction == null) {
            acceptChangesAction = new AcceptChangesAction();
        }
        return acceptChangesAction;
    }

    public class AcceptChangesAction extends AbstractAction {
        private static final long serialVersionUID = -8058749276509227718L;

        public void actionPerformed(ActionEvent ev) {
            accept();
        }
    }

    private CancelChangesAction getCancelChangesAction() {
        if (cancelChangesAction == null) {
            cancelChangesAction = new CancelChangesAction();
        }
        return cancelChangesAction;
    }

    private class CancelChangesAction extends WindowAdapter implements ActionListener {

        public void windowOpened(WindowEvent ev) {
        }

        public void actionPerformed(ActionEvent ev) {
            exit();
        }

        public void windowClosing(WindowEvent we) {
            exit();
        }
    }

    private SelectableNode<String> getArchetypeNode() {
        if (archetypeNode == null) {
            archetypeNode = new SelectableNodeBuilder<String>()
                    .setName(OpenEHRLanguageManager.getMessage("Archetypes"))
                    .setIcon(Archetypes.ICON)
                    .createSelectableNode();
            Collection<String> archetypeIds = archetypeManager.getArchetypes().getAllIds();
            insertArchetypeNodes(archetypeNode, archetypeIds, OpenEHRConst.OBSERVATION);
            insertArchetypeNodes(archetypeNode, archetypeIds, OpenEHRConst.ACTION);
            insertArchetypeNodes(archetypeNode, archetypeIds, OpenEHRConst.EVALUATION);
            insertArchetypeNodes(archetypeNode, archetypeIds, OpenEHRConst.INSTRUCTION);
            insertArchetypeNodes(archetypeNode, archetypeIds, OpenEHRConst.ENTRY);
        }
        return archetypeNode;
    }

    private static void insertArchetypeNodes(SelectableNode<String> root, Collection<String> archetypeIds, String rmName) {

        SelectableNode<Object> entryRoot = new SelectableNodeBuilder<>()
                .setName(OpenEHRConstUI.getName(rmName))
                .setDescription(OpenEHRConstUI.getDescription(rmName))
                .setIcon(OpenEHRConstUI.getIcon(rmName))
                .createSelectableNode();
        for (String archetypeId : archetypeIds) {
            String entryType = Archetypes.getEntryType(archetypeId);
            if (entryType != null && entryType.equals(rmName)) {
                SelectableNode<String> rnode = new SelectableNodeBuilder<String>()
                        .setName(archetypeId)
                        .setDescription(archetypeId)
                        .setIcon(Archetypes.getIcon(archetypeId))
                        .setObject(archetypeId)
                        .createSelectableNode();
                entryRoot.add(rnode);
            }
        }
        root.add(entryRoot);

    }

    private SelectableNode<String> getTemplateNode() {
        if (templateNode == null) {
            templateNode = generateTemplateNode();
        }
        return templateNode;
    }

    private SelectableNode<String> generateTemplateNode() {
        SelectableNode<String> templateNode = new SelectableNodeBuilder<String>()
                .setName(OpenEHRLanguageManager.getMessage("Templates"))
                .setIcon(Templates.ICON)
                .createSelectableNode();
        Collection<String> templateIds = archetypeManager.getTemplates().getAllIds();
        insertTemplateNodes(templateNode, templateIds);
        return templateNode;
    }

    private static void insertTemplateNodes(SelectableNode<String> rootNode, Collection<String> templateIds) {
        for (String templateId : templateIds) {
            SelectableNode<String> node = new SelectableNodeBuilder<String>()
                    .setName(templateId)
                    .setObject(templateId)
                    .setIcon(Templates.ICON)
                    .createSelectableNode();
            rootNode.add(node);
        }
    }

    public boolean getAnswer() {
        return _answer;
    }


    public CMElement getSelectedCMElement() {
        boolean isArchetypeTabSelected = getArchetypeTemplateTabbedPane().getSelectedIndex() == 0;
        String id = getSelectedId(isArchetypeTabSelected);
        if (id == null) {
            return null;
        } else {
            return getCmElement(id, isArchetypeTabSelected);
        }
    }

    private CMElement getCmElement(String id, boolean isArchetypeTabSelected) {
        if (isArchetypeTabSelected) {
            return archetypeManager.getArchetypes().getCMElement(id);
        } else {
            return archetypeManager.getTemplates().getCMElement(id);
        }
    }

    private String getSelectedId(boolean isArchetypeTabSelected) {
        if (isArchetypeTabSelected) {
            return NodeConversor.getSelectedElement(getArchetypeNode());
        } else {
            return NodeConversor.getSelectedElement(getTemplateNode());
        }
    }

    public String getSelectedDomain() {
        String idDomain = (String) getDomainSelector().getSelectedItem();
        if (idDomain.equals(ANY_DOMAIN)) {
            idDomain = null;
        }
        return idDomain;
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