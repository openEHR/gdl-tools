
package se.cambio.cds.gdl.editor.view.dialog;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.panels.FileSelectionPanel;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.view.dialogs.DialogEditor;

import javax.swing.*;
import java.awt.*;
import java.io.File;
/**
 * @author icorram
 *


 */
public class DialogRepositoriesPaths extends DialogEditor {

    /**
     *
     */
    private static final long serialVersionUID = 2562412853124970610L;
    private JFileChooser _archetypesFolderChooser = null;
    private JFileChooser _templatesFolderChooser = null;
    private JFileChooser _terminologiesFolderChooser = null;
    private JFileChooser _ontologiesFolderChooser = null;
    private JFileChooser _guidesFolderChooser = null;

    private FileSelectionPanel archetypesRepositoryFolderPanel = null;
    private FileSelectionPanel templatesRepositoryFolderPanel = null;
    private FileSelectionPanel terminologiesRepositoryFolderPanel = null;
    private FileSelectionPanel ontologiesRepositoryFolderPanel = null;
    private FileSelectionPanel guidesRepositoryFolderPanel = null;

    //private String _archetypeFolderStr = null;
    //private String _templateFolderStr = null;

    /**
     * This is the default constructor
     */
    public DialogRepositoriesPaths() {
        super(EditorManager.getActiveEditorWindow(),
                GDLEditorLanguageManager.getMessage("SelectRepositories"),
                new Dimension(500, 380),true);
        initialize();
    }

    /**
     * This method initializes this
     */
    private  void initialize() {
        getJPanel().setLayout(new BorderLayout());
        JPanel panelAux = new JPanel();
        panelAux.setLayout(new BoxLayout(panelAux, BoxLayout.Y_AXIS));
        getJPanel().add(panelAux, BorderLayout.NORTH);
        panelAux.add(Box.createVerticalStrut(5));
        panelAux.add(getArchetypeRepositoryFolderPanel());
        panelAux.add(getTemplateRepositoryFolderPanel());
        panelAux.add(getTerminologiesRepositoryFolderPanel());
        panelAux.add(getOntologiesRepositoryFolderPanel());
        panelAux.add(getGuidesRepositoryFolderPanel());
        JPanel panelAux2 = new JPanel(new FlowLayout(FlowLayout.CENTER));
        panelAux2.add(getAcceptButton());
        panelAux2.add(getCancelButton());
        getJPanel().add(panelAux2, BorderLayout.SOUTH);
    }

    private FileSelectionPanel getArchetypeRepositoryFolderPanel(){
        if (archetypesRepositoryFolderPanel==null){
            archetypesRepositoryFolderPanel = new FileSelectionPanel(getArchetypeFolderChooser());
            archetypesRepositoryFolderPanel.setBorder(
                    BorderFactory.createTitledBorder(GDLEditorLanguageManager.getMessage("ArchetypeRepository")));

        }
        return archetypesRepositoryFolderPanel;
    }

    public JFileChooser getArchetypeFolderChooser(){
        if (_archetypesFolderChooser==null){
            _archetypesFolderChooser = new JFileChooser();
            String path = UserConfigurationManager.getParameterWithDefault(UserConfigurationManager.ARCHETYPES_FOLDER_KW);
            _archetypesFolderChooser.setSelectedFile(new File(path));
            _archetypesFolderChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            _archetypesFolderChooser.setDialogTitle(GDLEditorLanguageManager.getMessage("SelectArchetypeRepository"));
        }
        return _archetypesFolderChooser;
    }

    private FileSelectionPanel getTemplateRepositoryFolderPanel(){
        if (templatesRepositoryFolderPanel==null){
            templatesRepositoryFolderPanel = new FileSelectionPanel(getTemplateFolderChooser());
            templatesRepositoryFolderPanel.setBorder(
                    BorderFactory.createTitledBorder(GDLEditorLanguageManager.getMessage("TemplateRepository")));

        }
        return templatesRepositoryFolderPanel;
    }

    public JFileChooser getTemplateFolderChooser(){
        if (_templatesFolderChooser==null){
            _templatesFolderChooser = new JFileChooser();
            _templatesFolderChooser.setSelectedFile(new File(UserConfigurationManager.getParameterWithDefault(UserConfigurationManager.TEMPLATES_FOLDER_KW)));
            _templatesFolderChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            _templatesFolderChooser.setDialogTitle(GDLEditorLanguageManager.getMessage("SelectTemplateRepository"));
        }
        return _templatesFolderChooser;
    }

    private FileSelectionPanel getTerminologiesRepositoryFolderPanel(){
        if (terminologiesRepositoryFolderPanel==null){
            terminologiesRepositoryFolderPanel = new FileSelectionPanel(getTerminologiesFolderChooser());
            terminologiesRepositoryFolderPanel.setBorder(
                    BorderFactory.createTitledBorder(GDLEditorLanguageManager.getMessage("TerminologiesRepository")));

        }
        return terminologiesRepositoryFolderPanel;
    }

    public JFileChooser getTerminologiesFolderChooser(){
        if (_terminologiesFolderChooser==null){
            _terminologiesFolderChooser = new JFileChooser();
            String path = UserConfigurationManager.getParameterWithDefault(UserConfigurationManager.TERMINOLOGIES_FOLDER_KW);
            _terminologiesFolderChooser.setSelectedFile(new File(path));
            _terminologiesFolderChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            _terminologiesFolderChooser.setDialogTitle(GDLEditorLanguageManager.getMessage("SelectTerminologiesRepository"));
        }
        return _terminologiesFolderChooser;
    }

    private FileSelectionPanel getOntologiesRepositoryFolderPanel(){
        if (ontologiesRepositoryFolderPanel==null){
            ontologiesRepositoryFolderPanel = new FileSelectionPanel(getOntologiesFolderChooser());
            ontologiesRepositoryFolderPanel.setBorder(
                    BorderFactory.createTitledBorder(GDLEditorLanguageManager.getMessage("OntologiesRepository")));

        }
        return ontologiesRepositoryFolderPanel;
    }

    public JFileChooser getOntologiesFolderChooser(){
        if (_ontologiesFolderChooser==null){
            _ontologiesFolderChooser = new JFileChooser();
            _ontologiesFolderChooser.setSelectedFile(new File(UserConfigurationManager.getParameterWithDefault(UserConfigurationManager.ONTOLOGIES_FOLDER_KW)));
            _ontologiesFolderChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            _ontologiesFolderChooser.setDialogTitle(GDLEditorLanguageManager.getMessage("SelectOntologiesRepository"));
        }
        return _ontologiesFolderChooser;
    }

    private FileSelectionPanel getGuidesRepositoryFolderPanel(){
        if (guidesRepositoryFolderPanel==null){
            guidesRepositoryFolderPanel = new FileSelectionPanel(getGuidesFolderChooser());
            guidesRepositoryFolderPanel.setBorder(
                    BorderFactory.createTitledBorder(GDLEditorLanguageManager.getMessage("GuidelinesRepository")));

        }
        return guidesRepositoryFolderPanel;
    }

    public JFileChooser getGuidesFolderChooser(){
        if (_guidesFolderChooser==null){
            _guidesFolderChooser = new JFileChooser();
            _guidesFolderChooser.setSelectedFile(new File(UserConfigurationManager.getParameterWithDefault(UserConfigurationManager.GUIDES_FOLDER_KW)));
            _guidesFolderChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            _guidesFolderChooser.setDialogTitle(GDLEditorLanguageManager.getMessage("SelectGuidesRepository"));
        }
        return _guidesFolderChooser;
    }


    protected boolean acceptDialog(){
        String archetypeFolderStrSelected = getArchetypeRepositoryFolderPanel().getFileNameJTextField().getText();
        String templateFolderStrSelected = getTemplateRepositoryFolderPanel().getFileNameJTextField().getText();
        String terminologiesFolderStrSelected = getTerminologiesRepositoryFolderPanel().getFileNameJTextField().getText();
        String guidesFolderStrSelected = getGuidesRepositoryFolderPanel().getFileNameJTextField().getText();
        String ontologiesFolderStrSelected = getOntologiesRepositoryFolderPanel().getFileNameJTextField().getText();
        UserConfigurationManager.setParameterWithDefault(UserConfigurationManager.ARCHETYPES_FOLDER_KW, archetypeFolderStrSelected);
        UserConfigurationManager.setParameterWithDefault(UserConfigurationManager.TEMPLATES_FOLDER_KW, templateFolderStrSelected);
        UserConfigurationManager.setParameterWithDefault(UserConfigurationManager.TERMINOLOGIES_FOLDER_KW, terminologiesFolderStrSelected);
        UserConfigurationManager.setParameterWithDefault(UserConfigurationManager.GUIDES_FOLDER_KW, guidesFolderStrSelected);
        UserConfigurationManager.setParameterWithDefault(UserConfigurationManager.ONTOLOGIES_FOLDER_KW, ontologiesFolderStrSelected);
        JOptionPane.showMessageDialog(EditorManager.getActiveEditorWindow(), GDLEditorLanguageManager.getMessage("MustRestartForChangesToTakeEffect"));
        return UserConfigurationManager.saveConfig();
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