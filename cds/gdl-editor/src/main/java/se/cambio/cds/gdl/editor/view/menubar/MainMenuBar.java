
package se.cambio.cds.gdl.editor.view.menubar;

import java.util.HashMap;
import java.util.Map;

import javax.swing.Action;
import javax.swing.ButtonGroup;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JRadioButtonMenuItem;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;

/**
 * @author iago.corbal
 *


 */
public class MainMenuBar extends JMenuBar {


    /**
     * Comentario para <code>serialVersionUID</code>
     */
    private static final long serialVersionUID = 1L;
    private JMenu fileMenu;
    private Action loadGuideAction;
    private Action saveGuideAction;
    private Action saveGuideAsAction;
    private Action saveGuideAsObjectAction;
    private Action exitEditorAction;
    private Action createNewGuideAction;
    private JMenu languageMenu;
    private JMenu configurationMenu;
    private Action addLanguageToGuideAction;
    private Action configRepositoriesAction;
    private Action currentDateAndTimeAction;
    private Action importArchetypeAction;
    private Action importTemplateAction;
    private JMenu exportMenu;
    private JMenu importMenu;
    private JMenu helpMenu;
    private Action exportToHTMLAction;
    private Action viewSamplesAction;
    private Action aboutGDLEditorAction;
    private Action releaseNotesAction;
    private Action viewUserManualAction;
    private Map<String, JRadioButtonMenuItem> _languageRadioButtonMenuItems = null;

    public MainMenuBar(){
	initialize();
    }		


    /**
     * This method initializes this
     */
    private void initialize() {
	this.add(getFileMenu());
	this.add(getLanguageMenu());
	this.add(getConfigurationMenu());
	this.add(getHelpMenu());
    }

    public JMenu getFileMenu() {
	if (fileMenu == null) {
	    fileMenu = new JMenu();
	    fileMenu.setText(GDLEditorLanguageManager.getMessage("File"));
	    fileMenu.add(getCreateNewGuideAction());
	    fileMenu.add(getLoadGuideAction());
	    fileMenu.add(getSaveGuideAction());
	    fileMenu.add(getSaveGuideAsAction());
	    fileMenu.add(getImportMenu());
	    fileMenu.add(getExportMenu());
	    fileMenu.add(getExitEditorAction());
	}
	return fileMenu;
    }

    public JMenu getImportMenu(){
	if (importMenu ==null){
	    importMenu = new JMenu(GDLEditorLanguageManager.getMessage("Import"));
	    importMenu.add(getImportArchetypeAction());
	    importMenu.add(getImportTemplateAction());
	}
	return importMenu;
    }

    public JMenu getExportMenu(){
	if (exportMenu ==null){
	    exportMenu = new JMenu(GDLEditorLanguageManager.getMessage("Export"));
	    exportMenu.add(getSaveGuideAsObjectAction());
	    exportMenu.add(getExportToHTMLAction());
	}
	return exportMenu;
    }
    
    public Action getCreateNewGuideAction(){
	if (createNewGuideAction ==null){
	    createNewGuideAction = new NewGuideAction();
	}
	return createNewGuideAction;
    }

    public Action getLoadGuideAction(){
	if (loadGuideAction ==null){
	    loadGuideAction = new LoadGuideAction();
	}
	return loadGuideAction;
    }

    public Action getSaveGuideAction(){
	if (saveGuideAction ==null){
	    saveGuideAction = new SaveGuideAction();
	}
	return saveGuideAction;
    }

    public Action getSaveGuideAsAction(){
	if (saveGuideAsAction ==null){
	    saveGuideAsAction = new SaveGuideAsAction();
	}
	return saveGuideAsAction;
    }

    public Action getSaveGuideAsObjectAction(){
	if (saveGuideAsObjectAction ==null){
	    saveGuideAsObjectAction = new SaveGuideAsObjectAction();
	}
	return saveGuideAsObjectAction;
    }

    public Action getExitEditorAction(){
	if (exitEditorAction ==null){
	    exitEditorAction = new ExitEditorAction();
	}
	return exitEditorAction;
    }
    
    public Action getImportArchetypeAction(){
	if (importArchetypeAction ==null){
	    importArchetypeAction = new ImportArchetypeAction();
	}
	return importArchetypeAction;
    }
    
    public Action getImportTemplateAction(){
	if (importTemplateAction ==null){
	    importTemplateAction = new ImportTemplateAction();
	}
	return importTemplateAction;
    }
    
    
    public Action getExportToHTMLAction(){
	if (exportToHTMLAction ==null){
	    exportToHTMLAction = new ExportToHTMLAction();
	}
	return exportToHTMLAction;
    }
    
    public JMenu getLanguageMenu() {
	if (languageMenu == null) {
	    languageMenu = new JMenu();
	    languageMenu.setText(GDLEditorLanguageManager.getMessage("Language"));
	}
	return languageMenu;
    }

    public void refreshLanguageMenu(){
	getLanguageMenu().removeAll();
	ButtonGroup itemGroup = new ButtonGroup();
	GDLEditor controller = EditorManager.getActiveGDLEditor();
	String currentLang= controller.getCurrentGuideLanguageCode();
	for (String languageCode : controller.getSupportedLanguageCodes()) {
	    JRadioButtonMenuItem rbMenuItem = 
		    new JRadioButtonMenuItem(new ChangeGuideLanguageAction(languageCode));
	    getLanguageRadioButtonMenuItemsMap().put(currentLang, rbMenuItem);
	    itemGroup.add(rbMenuItem);
	    getLanguageMenu().add(rbMenuItem);
	    if (languageCode.equals(currentLang)){
		rbMenuItem.setSelected(true);
	    }
	}
	getLanguageMenu().add(getAddLanguageToGuideAction());
    }

    private Map<String, JRadioButtonMenuItem> getLanguageRadioButtonMenuItemsMap(){
	if (_languageRadioButtonMenuItems==null){
	    _languageRadioButtonMenuItems = new HashMap<String, JRadioButtonMenuItem>();
	}
	return _languageRadioButtonMenuItems;
    }

    public JRadioButtonMenuItem getLanguageRadioButtonMenuItem(String lang){
	return getLanguageRadioButtonMenuItemsMap().get(lang);
    }

    public Action getAddLanguageToGuideAction(){
	if (addLanguageToGuideAction ==null){
	    addLanguageToGuideAction = new AddLanguageToGuideAction();
	}
	return addLanguageToGuideAction;
    }
    
    public JMenu getConfigurationMenu() {
	if (configurationMenu == null) {
	    configurationMenu = new JMenu();
	    configurationMenu.setText(GDLEditorLanguageManager.getMessage("Configuration"));
	    configurationMenu.add(getConfigurationRepositoriesAction());
	    configurationMenu.add(getCurrentDateAndTimeAction());
	    configurationMenu.add(new ConfigLanguageAction());
	}
	return configurationMenu;
    }
    
    public Action getConfigurationRepositoriesAction(){
	if (configRepositoriesAction ==null){
	    configRepositoriesAction = new ConfigRepositoriesAction();
	}
	return configRepositoriesAction;
    }
    
    public Action getCurrentDateAndTimeAction(){
	if (currentDateAndTimeAction ==null){
	    currentDateAndTimeAction = new CurrentDateAndTimeAction();
	}
	return currentDateAndTimeAction;
    }
    
    public JMenu getHelpMenu() {
	if (helpMenu == null) {
	    helpMenu = new JMenu();
	    helpMenu.setText(GDLEditorLanguageManager.getMessage("Help"));
	    helpMenu.add(getViewUserManualAction());
	    helpMenu.add(getViewSamplesAction());
	    helpMenu.add(getReleaseNotesAction());
	    helpMenu.addSeparator();
	    helpMenu.add(getAboutGDLEditorAction());
	}
	return helpMenu;
    }
    
    public Action getViewUserManualAction(){
	if (viewUserManualAction ==null){
	    viewUserManualAction = new ViewUserManualAction();
	}
	return viewUserManualAction;
    }
    
    public Action getViewSamplesAction(){
	if (viewSamplesAction ==null){
	    viewSamplesAction = new ViewSamplesAction();
	}
	return viewSamplesAction;
    }

    public Action getReleaseNotesAction(){
	if (releaseNotesAction ==null){
	    releaseNotesAction = new ViewReleaseNotesMenuAction();
	}
	return releaseNotesAction;
    }
    
    
    public Action getAboutGDLEditorAction(){
	if (aboutGDLEditorAction ==null){
	    aboutGDLEditorAction = new AboutGDLEditorMenuAction();
	}
	return aboutGDLEditorAction;
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