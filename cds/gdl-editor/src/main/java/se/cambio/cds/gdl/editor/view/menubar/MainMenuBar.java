package se.cambio.cds.gdl.editor.view.menubar;

import se.cambio.cds.gdl.editor.controller.*;
import se.cambio.cds.gdl.editor.controller.interfaces.EditorController;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.util.export.html.GuideHTMLExporter;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.view.util.ImportManager;

import javax.swing.*;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public class MainMenuBar extends JMenuBar {

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
    private Map<String, JRadioButtonMenuItem> languageRadioButtonMenuItems = null;
    private ImportManager importManager;
    private GuideHTMLExporter guideHTMLExporter;
    private EditorManager editorManager;
    private GdlEditorFactory gdlEditorFactory;
    private EditorFileManager editorFileManager;
    private GuidelineLoadManager guidelineLoadManager;
    private UserConfigurationManager userConfigurationManager;

    public MainMenuBar(
            ImportManager importManager,
            GuideHTMLExporter guideHTMLExporter,
            EditorManager editorManager,
            GdlEditorFactory gdlEditorFactory,
            EditorFileManager editorFileManager,
            GuidelineLoadManager guidelineLoadManager,
            UserConfigurationManager userConfigurationManager) {
        this.importManager = importManager;
        this.guideHTMLExporter = guideHTMLExporter;
        this.editorManager = editorManager;
        this.gdlEditorFactory = gdlEditorFactory;
        this.editorFileManager = editorFileManager;
        this.guidelineLoadManager = guidelineLoadManager;
        this.userConfigurationManager = userConfigurationManager;
        initialize();
    }


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

    private JMenu getImportMenu() {
        if (importMenu == null) {
            importMenu = new JMenu(GDLEditorLanguageManager.getMessage("Import"));
            importMenu.add(getImportArchetypeAction());
            importMenu.add(getImportTemplateAction());
        }
        return importMenu;
    }

    private JMenu getExportMenu() {
        if (exportMenu == null) {
            exportMenu = new JMenu(GDLEditorLanguageManager.getMessage("Export"));
            exportMenu.add(getSaveGuideAsObjectAction());
            exportMenu.add(getExportToHTMLAction());
        }
        return exportMenu;
    }

    private Action getCreateNewGuideAction() {
        if (createNewGuideAction == null) {
            createNewGuideAction = new NewGuideAction(editorManager, editorFileManager, gdlEditorFactory, this);
        }
        return createNewGuideAction;
    }

    private Action getLoadGuideAction() {
        if (loadGuideAction == null) {
            loadGuideAction = new LoadGuideAction(editorManager, guidelineLoadManager);
        }
        return loadGuideAction;
    }

    private Action getSaveGuideAction() {
        if (saveGuideAction == null) {
            saveGuideAction = new SaveGuideAction(editorManager);
        }
        return saveGuideAction;
    }

    private Action getSaveGuideAsAction() {
        if (saveGuideAsAction == null) {
            saveGuideAsAction = new SaveGuideAsAction(editorManager);
        }
        return saveGuideAsAction;
    }

    private Action getSaveGuideAsObjectAction() {
        if (saveGuideAsObjectAction == null) {
            saveGuideAsObjectAction = new SaveGuideAsObjectAction(editorManager);
        }
        return saveGuideAsObjectAction;
    }

    private Action getExitEditorAction() {
        if (exitEditorAction == null) {
            exitEditorAction = new ExitEditorAction(editorManager);
        }
        return exitEditorAction;
    }

    private Action getImportArchetypeAction() {
        if (importArchetypeAction == null) {
            importArchetypeAction = new ImportArchetypeAction(importManager, editorManager);
        }
        return importArchetypeAction;
    }

    private Action getImportTemplateAction() {
        if (importTemplateAction == null) {
            importTemplateAction = new ImportTemplateAction(importManager, editorManager);
        }
        return importTemplateAction;
    }


    private Action getExportToHTMLAction() {
        if (exportToHTMLAction == null) {
            exportToHTMLAction = new ExportToHTMLAction(guideHTMLExporter, editorManager);
        }
        return exportToHTMLAction;
    }

    private JMenu getLanguageMenu() {
        if (languageMenu == null) {
            languageMenu = new JMenu();
            languageMenu.setText(GDLEditorLanguageManager.getMessage("Language"));
        }
        return languageMenu;
    }

    public void refreshLanguageMenu() {
        getLanguageMenu().removeAll();
        ButtonGroup itemGroup = new ButtonGroup();
        EditorController controller = editorManager.getActiveEditorController();
        assert controller != null;
        String currentLang = controller.getCurrentLanguageCode();
        Collection<String> supportedLanguageCodes = controller.getSupportedLanguageCodes();
        for (String languageCode : supportedLanguageCodes) {
            JRadioButtonMenuItem rbMenuItem =
                    new JRadioButtonMenuItem(new ChangeGuideLanguageAction(languageCode, editorManager, this));
            getLanguageRadioButtonMenuItemsMap().put(currentLang, rbMenuItem);
            itemGroup.add(rbMenuItem);
            getLanguageMenu().add(rbMenuItem);
            if (languageCode.equals(currentLang)) {
                rbMenuItem.setSelected(true);
            }
        }
        getLanguageMenu().add(getAddLanguageToGuideAction());
    }

    private Map<String, JRadioButtonMenuItem> getLanguageRadioButtonMenuItemsMap() {
        if (languageRadioButtonMenuItems == null) {
            languageRadioButtonMenuItems = new HashMap<>();
        }
        return languageRadioButtonMenuItems;
    }

    private Action getAddLanguageToGuideAction() {
        if (addLanguageToGuideAction == null) {
            addLanguageToGuideAction = new AddLanguageToGuideAction(this, editorManager);
        }
        return addLanguageToGuideAction;
    }

    private JMenu getConfigurationMenu() {
        if (configurationMenu == null) {
            configurationMenu = new JMenu();
            configurationMenu.setText(GDLEditorLanguageManager.getMessage("Configuration"));
            configurationMenu.add(getConfigurationRepositoriesAction());
            configurationMenu.add(getCurrentDateAndTimeAction());
            configurationMenu.add(new ConfigLanguageAction(editorManager, userConfigurationManager));
        }
        return configurationMenu;
    }

    private Action getConfigurationRepositoriesAction() {
        if (configRepositoriesAction == null) {
            configRepositoriesAction = new ConfigRepositoriesAction(editorManager, userConfigurationManager);
        }
        return configRepositoriesAction;
    }

    private Action getCurrentDateAndTimeAction() {
        if (currentDateAndTimeAction == null) {
            currentDateAndTimeAction = new CurrentDateAndTimeAction(editorManager, userConfigurationManager);
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

    private Action getViewUserManualAction() {
        if (viewUserManualAction == null) {
            viewUserManualAction = new ViewUserManualAction(userConfigurationManager);
        }
        return viewUserManualAction;
    }

    private Action getViewSamplesAction() {
        if (viewSamplesAction == null) {
            viewSamplesAction = new ViewSamplesAction(userConfigurationManager);
        }
        return viewSamplesAction;
    }

    private Action getReleaseNotesAction() {
        if (releaseNotesAction == null) {
            releaseNotesAction = new ViewReleaseNotesMenuAction(userConfigurationManager);
        }
        return releaseNotesAction;
    }


    private Action getAboutGDLEditorAction() {
        if (aboutGDLEditorAction == null) {
            aboutGDLEditorAction = new AboutGDLEditorMenuAction(editorManager);
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