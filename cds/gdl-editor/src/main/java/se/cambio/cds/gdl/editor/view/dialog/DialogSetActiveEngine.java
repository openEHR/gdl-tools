
package se.cambio.cds.gdl.editor.view.dialog;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.view.dialogs.DialogEditor;

import javax.swing.*;
import java.awt.*;

import static java.lang.String.format;


public class DialogSetActiveEngine extends DialogEditor {

    private static final long serialVersionUID = 2562412853124970610L;
    private JComboBox<String> engineComboBox = null;
    private Logger logger = LoggerFactory.getLogger(DialogSetActiveEngine.class);
    private EditorManager editorManager;
    private UserConfigurationManager userConfigurationManager;

    public DialogSetActiveEngine(EditorManager editorManager, UserConfigurationManager userConfigurationManager) {
        super(editorManager.getActiveEditorWindow(),
                GDLEditorLanguageManager.getMessage("SetActiveEngine"),
                new Dimension(300, 110), true);
        this.editorManager = editorManager;
        this.userConfigurationManager = userConfigurationManager;
        initialize();
    }

    private void initialize() {
        getJPanel().setLayout(new BorderLayout());
        JPanel panelAux = new JPanel(new BorderLayout());
        getJPanel().add(panelAux, BorderLayout.NORTH);

        JPanel panelAux1 = new JPanel(new FlowLayout(FlowLayout.CENTER));
        panelAux1.add(new JLabel(GDLEditorLanguageManager.getMessage("SetActiveEngine") + ":"));
        panelAux1.add(getEngineComboBox());
        panelAux.add(panelAux1, BorderLayout.NORTH);
        JPanel panelAux2 = new JPanel(new FlowLayout(FlowLayout.CENTER));
        panelAux2.add(getAcceptButton());
        panelAux2.add(getCancelButton());
        getJPanel().add(panelAux2, BorderLayout.SOUTH);

    }

    private JComboBox getEngineComboBox() {
        if (engineComboBox == null) {
            engineComboBox = new JComboBox<>();
            for (String ruleEngine : userConfigurationManager.getSupportedRuleEngines()) {
                engineComboBox.addItem(ruleEngine);
            }
            String activeRuleEngine = userConfigurationManager.getActiveRuleEngine();
            if (userConfigurationManager.getSupportedRuleEngines().contains(activeRuleEngine)) {
                engineComboBox.setSelectedItem(activeRuleEngine);
            }
        }
        return engineComboBox;
    }


    protected boolean acceptDialog() {
        String activeRuleEngine = (String) getEngineComboBox().getSelectedItem();
        userConfigurationManager.setActiveRuleEngine(activeRuleEngine);
        JOptionPane.showMessageDialog(editorManager.getActiveEditorWindow(), GDLEditorLanguageManager.getMessage("MustRestartForChangesToTakeEffect"));
        try {
            userConfigurationManager.saveConfig();
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