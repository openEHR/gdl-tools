
package se.cambio.cds.gdl.editor.view.dialog;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.panels.FileSelectionPanel;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.view.dialogs.DialogEditor;

import javax.swing.*;
import java.awt.*;

import static java.lang.String.format;

public class DialogRepositoriesPaths extends DialogEditor {

    private static final long serialVersionUID = 2562412853124970610L;
    private JFileChooser archetypesFolderChooser = null;
    private JFileChooser templatesFolderChooser = null;
    private JFileChooser terminologiesFolderChooser = null;
    private JFileChooser guidesFolderChooser = null;

    private FileSelectionPanel archetypesRepositoryFolderPanel = null;
    private FileSelectionPanel templatesRepositoryFolderPanel = null;
    private FileSelectionPanel terminologiesRepositoryFolderPanel = null;
    private FileSelectionPanel guidesRepositoryFolderPanel = null;
    private Logger logger = LoggerFactory.getLogger(DialogRepositoriesPaths.class);

    public DialogRepositoriesPaths() {
        super(EditorManager.getActiveEditorWindow(),
                GDLEditorLanguageManager.getMessage("SelectRepositories"),
                new Dimension(500, 380), true);
        initialize();
    }

    private void initialize() {
        getJPanel().setLayout(new BorderLayout());
        JPanel panelAux = new JPanel();
        panelAux.setLayout(new BoxLayout(panelAux, BoxLayout.Y_AXIS));
        getJPanel().add(panelAux, BorderLayout.NORTH);
        panelAux.add(Box.createVerticalStrut(5));
        panelAux.add(getArchetypeRepositoryFolderPanel());
        panelAux.add(getTemplateRepositoryFolderPanel());
        panelAux.add(getTerminologiesRepositoryFolderPanel());
        panelAux.add(getGuidesRepositoryFolderPanel());
        JPanel panelAux2 = new JPanel(new FlowLayout(FlowLayout.CENTER));
        panelAux2.add(getAcceptButton());
        panelAux2.add(getCancelButton());
        getJPanel().add(panelAux2, BorderLayout.SOUTH);
    }

    private FileSelectionPanel getArchetypeRepositoryFolderPanel() {
        if (archetypesRepositoryFolderPanel == null) {
            archetypesRepositoryFolderPanel = new FileSelectionPanel(getArchetypeFolderChooser());
            archetypesRepositoryFolderPanel.setBorder(
                    BorderFactory.createTitledBorder(GDLEditorLanguageManager.getMessage("ArchetypeRepository")));

        }
        return archetypesRepositoryFolderPanel;
    }

    private JFileChooser getArchetypeFolderChooser() {
        if (archetypesFolderChooser == null) {
            archetypesFolderChooser = new JFileChooser();
            archetypesFolderChooser.setSelectedFile(UserConfigurationManager.instance().getArchetypeFolder().getFolder());
            archetypesFolderChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            archetypesFolderChooser.setDialogTitle(GDLEditorLanguageManager.getMessage("SelectArchetypeRepository"));
        }
        return archetypesFolderChooser;
    }

    private FileSelectionPanel getTemplateRepositoryFolderPanel() {
        if (templatesRepositoryFolderPanel == null) {
            templatesRepositoryFolderPanel = new FileSelectionPanel(getTemplateFolderChooser());
            templatesRepositoryFolderPanel.setBorder(
                    BorderFactory.createTitledBorder(GDLEditorLanguageManager.getMessage("TemplateRepository")));

        }
        return templatesRepositoryFolderPanel;
    }

    private JFileChooser getTemplateFolderChooser() {
        if (templatesFolderChooser == null) {
            templatesFolderChooser = new JFileChooser();
            templatesFolderChooser.setSelectedFile(UserConfigurationManager.instance().getTemplateFolder().getFolder());
            templatesFolderChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            templatesFolderChooser.setDialogTitle(GDLEditorLanguageManager.getMessage("SelectTemplateRepository"));
        }
        return templatesFolderChooser;
    }

    private FileSelectionPanel getTerminologiesRepositoryFolderPanel() {
        if (terminologiesRepositoryFolderPanel == null) {
            terminologiesRepositoryFolderPanel = new FileSelectionPanel(getTerminologiesFolderChooser());
            terminologiesRepositoryFolderPanel.setBorder(
                    BorderFactory.createTitledBorder(GDLEditorLanguageManager.getMessage("TerminologiesRepository")));

        }
        return terminologiesRepositoryFolderPanel;
    }

    private JFileChooser getTerminologiesFolderChooser() {
        if (terminologiesFolderChooser == null) {
            terminologiesFolderChooser = new JFileChooser();
            terminologiesFolderChooser.setSelectedFile(UserConfigurationManager.instance().getTerminologiesFolder().getFolder());
            terminologiesFolderChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            terminologiesFolderChooser.setDialogTitle(GDLEditorLanguageManager.getMessage("SelectTerminologiesRepository"));
        }
        return terminologiesFolderChooser;
    }

    private FileSelectionPanel getGuidesRepositoryFolderPanel() {
        if (guidesRepositoryFolderPanel == null) {
            guidesRepositoryFolderPanel = new FileSelectionPanel(getGuidesFolderChooser());
            guidesRepositoryFolderPanel.setBorder(
                    BorderFactory.createTitledBorder(GDLEditorLanguageManager.getMessage("GuidelinesRepository")));

        }
        return guidesRepositoryFolderPanel;
    }

    private JFileChooser getGuidesFolderChooser() {
        if (guidesFolderChooser == null) {
            guidesFolderChooser = new JFileChooser();
            guidesFolderChooser.setSelectedFile(UserConfigurationManager.instance().getGuidesFolder().getFolder());
            guidesFolderChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            guidesFolderChooser.setDialogTitle(GDLEditorLanguageManager.getMessage("SelectGuidesRepository"));
        }
        return guidesFolderChooser;
    }


    protected boolean acceptDialog() {
        String archetypeFolderStrSelected = getArchetypeRepositoryFolderPanel().getFileNameJTextField().getText();
        String templateFolderStrSelected = getTemplateRepositoryFolderPanel().getFileNameJTextField().getText();
        String terminologiesFolderStrSelected = getTerminologiesRepositoryFolderPanel().getFileNameJTextField().getText();
        String guidesFolderStrSelected = getGuidesRepositoryFolderPanel().getFileNameJTextField().getText();
        try {
            UserConfigurationManager.instance().setArchetypesFolderPath(archetypeFolderStrSelected);
            UserConfigurationManager.instance().setTemplatesFolderPath(templateFolderStrSelected);
            UserConfigurationManager.instance().setTerminologiesFolderPath(terminologiesFolderStrSelected);
            UserConfigurationManager.instance().setGuidelinesFolderPath(guidesFolderStrSelected);
        } catch (Exception ex) {
            JOptionPane.showMessageDialog(EditorManager.getActiveEditorWindow(), "ERROR : " + ex.getMessage(), "ERROR", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        try {
            UserConfigurationManager.instance().saveConfig();
        } catch (Exception e) {
            logger.error("Error saving config file.", e);
            JOptionPane.showMessageDialog(null, format("Error saving config file: %s", e.getMessage()), "Error", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        return true;
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