
package se.cambio.cds.gdl.editor.view.dialog;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.applicationobjects.Languages;
import se.cambio.cds.gdl.editor.view.renderers.LanguageRenderer;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.view.dialogs.DialogEditor;

import javax.swing.*;
import java.awt.*;
import java.util.Set;

import static java.lang.String.format;


public class DialogSetLanguage extends DialogEditor {

    private static final long serialVersionUID = 2562412853124970610L;
    private JComboBox<String> languageSelection = null;
    private EditorManager editorManager;
    private UserConfigurationManager userConfigurationManager;

    public DialogSetLanguage(
            EditorManager editorManager,
            UserConfigurationManager userConfigurationManager) {
        super(editorManager.getActiveEditorWindow(),
                GDLEditorLanguageManager.getMessage("SetEditorLanguage"),
                new Dimension(250, 110), true);
        this.editorManager = editorManager;
        this.userConfigurationManager = userConfigurationManager;
        initialize();
    }

    private void initialize() {
        getJPanel().setLayout(new BorderLayout());
        JPanel panelAux = new JPanel(new BorderLayout());
        getJPanel().add(panelAux, BorderLayout.NORTH);

        JPanel panelAux1 = new JPanel(new FlowLayout(FlowLayout.CENTER));
        panelAux1.add(new JLabel(GDLEditorLanguageManager.getMessage("SetEditorLanguage") + ":"));
        panelAux1.add(getLanguageSelectorComboBox());
        panelAux.add(panelAux1, BorderLayout.NORTH);
        JPanel panelAux2 = new JPanel(new FlowLayout(FlowLayout.CENTER));
        panelAux2.add(getAcceptButton());
        panelAux2.add(getCancelButton());
        getJPanel().add(panelAux2, BorderLayout.SOUTH);

    }

    private JComboBox getLanguageSelectorComboBox() {
        if (languageSelection == null) {
            Set<String> supportedLanguages = Languages.getSupportedLanguages();
            languageSelection = new JComboBox<>(supportedLanguages.toArray(new String[supportedLanguages.size()]));
            languageSelection.setRenderer(new LanguageRenderer());
            String langCountryCode = userConfigurationManager.getLanguage() + "_" + userConfigurationManager.getCountryCode();
            if (supportedLanguages.contains(langCountryCode)) {
                languageSelection.setSelectedItem(langCountryCode);
            }
        }
        return languageSelection;
    }


    protected boolean acceptDialog() {
        String languageAndCountry = (String) getLanguageSelectorComboBox().getSelectedItem();
        String[] str = languageAndCountry.split("_");
        userConfigurationManager.setLanguage(str[0]);
        userConfigurationManager.setCountry(str[1]);
        try {
            userConfigurationManager.saveConfig();
        } catch (Exception ex) {
            JOptionPane.showMessageDialog(null, format("Error saving config file: %s", ex.getMessage()), "Error", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        JOptionPane.showMessageDialog(editorManager.getActiveEditorWindow(), GDLEditorLanguageManager.getMessage("MustRestartForChangesToTakeEffect"));
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