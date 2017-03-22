package se.cambio.cds.gdl.editor.view.dialog;

import com.toedter.calendar.JDateChooser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.panels.ClockPanel;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.view.dialogs.DialogEditor;

import javax.swing.*;
import java.awt.*;
import java.util.Date;

import static java.lang.String.format;

public class DialogCurrentTimeSelection extends DialogEditor {

    private static final long serialVersionUID = 2562412853124970610L;
    private ClockPanel _clockPanel = null;
    private JRadioButton radioButtonDefault = null;
    private JRadioButton radioButtonCustom = null;
    private JDateChooser dateChooser;
    private static String DEFAULT_FORMAT = "dd/MM/yyyy HH:mm:ss";
    private Logger logger = LoggerFactory.getLogger(DialogCurrentTimeSelection.class);

    public DialogCurrentTimeSelection() {
        super(EditorManager.getActiveEditorWindow(),
                GDLEditorLanguageManager.getMessage("DefaultDateTime"),
                new Dimension(500, 180), true);
        initialize();
    }

    private void initialize() {
        getJPanel().setLayout(new BorderLayout());
        JPanel panelAux = new JPanel(new BorderLayout());
        getJPanel().add(panelAux, BorderLayout.NORTH);

        JPanel autoPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        autoPanel.add(getRadioButtonDefault());
        autoPanel.add(getClockPanel());
        panelAux.add(autoPanel, BorderLayout.NORTH);

        JPanel manualPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        manualPanel.add(getRadioButtonCustom());
        manualPanel.add(getDateChooser());
        panelAux.add(manualPanel, BorderLayout.SOUTH);

        ButtonGroup group = new ButtonGroup();
        group.add(getRadioButtonDefault());
        group.add(getRadioButtonCustom());
        JPanel panelAux2 = new JPanel(new FlowLayout(FlowLayout.CENTER));
        panelAux2.add(getAcceptButton());
        panelAux2.add(getCancelButton());
        getJPanel().add(panelAux2, BorderLayout.SOUTH);
    }

    private JRadioButton getRadioButtonDefault() {
        if (radioButtonDefault == null) {
            radioButtonDefault = new JRadioButton();
            radioButtonDefault.setText(GDLEditorLanguageManager.getMessage("CurrentDateTime"));
            if (!UserConfigurationManager.instance().hasCustomCurrentDateTime()) {
                radioButtonDefault.setSelected(true);
            }
        }
        return radioButtonDefault;
    }

    private JRadioButton getRadioButtonCustom() {
        if (radioButtonCustom == null) {
            radioButtonCustom = new JRadioButton();
            radioButtonCustom.setText(GDLEditorLanguageManager.getMessage("CustomDateTime"));
            if (UserConfigurationManager.instance().hasCustomCurrentDateTime()) {
                radioButtonCustom.setSelected(true);
            }
        }
        return radioButtonCustom;
    }

    private ClockPanel getClockPanel() {
        if (_clockPanel == null) {
            _clockPanel = new ClockPanel();
        }
        return _clockPanel;
    }

    public JDateChooser getDateChooser() {
        if (dateChooser == null) {
            dateChooser = new JDateChooser(DEFAULT_FORMAT, "##/##/#### ##:##:##", '_');
            dateChooser.setIcon(OpenEHRImageUtil.CALENDAR_ICON);
            Date date = UserConfigurationManager.instance().getCurrentDateTime();
            if (date != null) {
                dateChooser.setDate(date);
            }
        }
        return dateChooser;
    }

    protected boolean cancelDialog() {
        getClockPanel().setOff();
        return true;
    }

    protected boolean acceptDialog() {
        getClockPanel().setOff();
        Date date = null;
        if (getRadioButtonCustom().isSelected()) {
            date = getDateChooser().getDate();
        }
        UserConfigurationManager.instance().setCurrentDateTime(date);
        try {
            UserConfigurationManager.instance().saveConfig();
        } catch (Exception e) {
            logger.error("Error saving config file", e);
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