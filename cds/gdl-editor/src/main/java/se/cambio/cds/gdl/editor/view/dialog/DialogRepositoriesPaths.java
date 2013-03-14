
package se.cambio.cds.gdl.editor.view.dialog;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.util.LanguageManager;
import se.cambio.cds.gdl.editor.view.panels.FileSelectionPanel;
import se.cambio.cds.openehr.view.dialogs.DialogEditor;
import se.cambio.cds.util.UserConfigurationManager;
/**
 * @author icorram
 *


 */
public class DialogRepositoriesPaths extends DialogEditor {

    /**
     * 
     */
    private static final long serialVersionUID = 2562412853124970610L;
    private JFileChooser _archetypeFolderChooser = null;
    private JFileChooser _templateFolderChooser = null;

    private FileSelectionPanel archetypeRepositoryFolderPanel = null;
    private FileSelectionPanel templateRepositoryFolderPanel = null;

    //private String _archetypeFolderStr = null;
    //private String _templateFolderStr = null;

    /**
     * This is the default constructor
     */
    public DialogRepositoriesPaths() {
	super(EditorManager.getActiveEditorWindow(),
		LanguageManager.getMessage("SelectRepositories"),
		new Dimension(500, 180),true);
	initialize();
    }

    /**
     * This method initializes this
     */
    private  void initialize() {
	getJPanel().setLayout(new BorderLayout());
	JPanel panelAux = new JPanel(new BorderLayout());
	getJPanel().add(panelAux, BorderLayout.NORTH);
	panelAux.add(getArchetypeRepositoryFolderPanel(), BorderLayout.NORTH);
	panelAux.add(getTemplateRepositoryFolderPanel(), BorderLayout.SOUTH);
	JPanel panelAux2 = new JPanel(new FlowLayout(FlowLayout.CENTER));
	panelAux2.add(getAcceptButton());
	panelAux2.add(getCancelButton());
	getJPanel().add(panelAux2, BorderLayout.SOUTH);
	//_archetypeFolderStr = UserConfigurationManager.getParameterWithDefault(UserConfigurationManager.ARCHETYPES_FOLDER_KW);
	//_templateFolderStr = UserConfigurationManager.getParameterWithDefault(UserConfigurationManager.TEMPLATES_FOLDER_KW);
    }

    private FileSelectionPanel getArchetypeRepositoryFolderPanel(){
	if (archetypeRepositoryFolderPanel==null){
	    archetypeRepositoryFolderPanel = new FileSelectionPanel(getArchetypeFolderChooser());
	    archetypeRepositoryFolderPanel.setBorder(
		    BorderFactory.createTitledBorder(LanguageManager.getMessage("ArchetypeRepository")));

	}
	return archetypeRepositoryFolderPanel;
    }

    private FileSelectionPanel getTemplateRepositoryFolderPanel(){
	if (templateRepositoryFolderPanel==null){
	    templateRepositoryFolderPanel = new FileSelectionPanel(getTemplateFolderChooser());
	    templateRepositoryFolderPanel.setBorder(
		    BorderFactory.createTitledBorder(LanguageManager.getMessage("TemplateRepository")));

	}
	return templateRepositoryFolderPanel;
    }

    public JFileChooser getArchetypeFolderChooser(){
	if (_archetypeFolderChooser==null){
	    _archetypeFolderChooser = new JFileChooser();
	    String path = UserConfigurationManager.getParameterWithDefault(UserConfigurationManager.ARCHETYPES_FOLDER_KW);
	    _archetypeFolderChooser.setSelectedFile(new File(path));
	    _archetypeFolderChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	    _archetypeFolderChooser.setDialogTitle(LanguageManager.getMessage("SelectArchetypeRepository"));
	}
	return _archetypeFolderChooser;
    }

    public JFileChooser getTemplateFolderChooser(){
	if (_templateFolderChooser==null){
	    _templateFolderChooser = new JFileChooser();
	    _templateFolderChooser.setSelectedFile(new File(UserConfigurationManager.getParameterWithDefault(UserConfigurationManager.TEMPLATES_FOLDER_KW)));
	    _templateFolderChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	    _templateFolderChooser.setDialogTitle(LanguageManager.getMessage("SelectTemplateRepository"));
	}
	return _templateFolderChooser;
    }

    protected boolean acceptDialog(){
	String archetypeFolderStrSelected = getArchetypeRepositoryFolderPanel().getNombreFicheroJTextField().getText();
	String templateFolderStrSelected = getTemplateRepositoryFolderPanel().getNombreFicheroJTextField().getText();
	UserConfigurationManager.setParameterWithDefault(UserConfigurationManager.ARCHETYPES_FOLDER_KW, archetypeFolderStrSelected);
	UserConfigurationManager.setParameterWithDefault(UserConfigurationManager.TEMPLATES_FOLDER_KW, templateFolderStrSelected);
	JOptionPane.showMessageDialog(EditorManager.getActiveEditorWindow(), LanguageManager.getMessage("MustRestartForChangesToTakeEffect"));
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