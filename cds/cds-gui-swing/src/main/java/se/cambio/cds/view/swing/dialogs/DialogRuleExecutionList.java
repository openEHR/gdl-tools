package se.cambio.cds.view.swing.dialogs;

import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.view.util.CollapsiblePanel;
import se.cambio.openehr.view.util.ScreenUtil;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

public class DialogRuleExecutionList extends JDialog {

    private static final long serialVersionUID = 1L;
    private JPanel jPanel;
    private Map<String, Map<String, String>> _rulesViewMap = null;
    private JComboBox<String> languageComboBox;
    private JPanel rulesPanel;
    private JPanel mainPanel;
    private Collection<Integer> expandedRules = new ArrayList<Integer>();
    private boolean _isAlert = false;
    private String language;

    public DialogRuleExecutionList(Window owner, Map<String, Map<String, String>> rulesViewMap, boolean isAlert, String language) {
        super(owner, ModalityType.APPLICATION_MODAL);
        _rulesViewMap = rulesViewMap;
        _isAlert = isAlert;
        this.language = language;
        init();
    }

    private void init() {
        this.setSize(new Dimension(600, 600));
        ScreenUtil.centerComponentOnScreen(this, this.getOwner());
        this.setResizable(true);
        this.setContentPane(getMainPanel());
        KeyStroke enter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, true);
        ExitAction exitAction = new ExitAction();
        getMainPanel().registerKeyboardAction(exitAction, enter, JComponent.WHEN_IN_FOCUSED_WINDOW);
        KeyStroke esc = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, true);
        getMainPanel().registerKeyboardAction(exitAction, esc, JComponent.WHEN_IN_FOCUSED_WINDOW);
    }

    private JPanel getMainPanel() {
        if (mainPanel == null) {
            mainPanel = new JPanel();
            mainPanel.setLayout(new BorderLayout());
            JPanel topPanel = new JPanel(new BorderLayout());
            JPanel auxPanel = new JPanel();
            auxPanel.add(new JLabel(OpenEHRLanguageManager.getMessage("Language") + ":"));
            auxPanel.add(getLanguageComboBox());
            topPanel.add(auxPanel, BorderLayout.EAST);
            if (_isAlert) {
                JLabel label = new JLabel(OpenEHRLanguageManager.getMessage("Alerts"));
                label.setIcon(OpenEHRImageUtil.WARNING_ICON);
                topPanel.add(label, BorderLayout.WEST);
            }

            mainPanel.add(topPanel, BorderLayout.NORTH);
            mainPanel.add(getJPanel(), BorderLayout.CENTER);
        }
        return mainPanel;
    }

    protected JPanel getJPanel() {
        if (jPanel == null) {
            jPanel = new JPanel();
            jPanel.setLayout(new BorderLayout());
            refresh();
        }
        return jPanel;
    }

    protected JPanel getRulesPanel() {
        if (rulesPanel == null) {
            rulesPanel = new JPanel();
            rulesPanel.setLayout(new BoxLayout(rulesPanel, BoxLayout.Y_AXIS));
            Map<String, String> rulesMap = _rulesViewMap.get(getSelectedLanguage());
            int i = 0;
            String ruleId;
            for (Map.Entry<String, String> entry : rulesMap.entrySet()) {
                ruleId = entry.getKey();
                CollapsiblePanel collapsiblePanel = new CollapsiblePanel(ruleId);
                collapsiblePanel.getActionButton().addActionListener(new CollapsablePanelActionListener(collapsiblePanel, i));
                if (expandedRules.contains(i)) {
                    collapsiblePanel.setCollapsed(false);
                }
                JPanel aux = collapsiblePanel.getContentPane();
                aux.setLayout(new BorderLayout());
                JEditorPane editorPane = new JEditorPane();
                editorPane.setContentType("text/html");
                String text = rulesMap.get(ruleId);
                text = text.replaceAll("(\r\n|\n)", "<br />");
                editorPane.setText("<HTML>" + text + "</HTML>");
                aux.add(editorPane, BorderLayout.CENTER);
                rulesPanel.add(collapsiblePanel);
                i++;
            }
        }
        return rulesPanel;
    }

    private class CollapsablePanelActionListener implements ActionListener {
        private CollapsiblePanel collapsiblePanel = null;
        private Integer index;

        public CollapsablePanelActionListener(CollapsiblePanel collapsiblePanel, int index) {
            this.collapsiblePanel = collapsiblePanel;
            this.index = index;
        }

        public void actionPerformed(ActionEvent ev) {
            if (collapsiblePanel.isCollapsed()) {
                expandedRules.add(index);
            } else {
                expandedRules.remove(index);
            }
        }
    }

    public String getSelectedLanguage() {
        return (String) getLanguageComboBox().getSelectedItem();
    }

    public JComboBox getLanguageComboBox() {
        if (languageComboBox == null) {
            languageComboBox = new JComboBox<>();
            for (String lang : _rulesViewMap.keySet()) {
                languageComboBox.addItem(lang);
            }
            languageComboBox.setSelectedItem(language);
            languageComboBox.addActionListener(e -> refresh());
        }
        return languageComboBox;
    }

    private void refresh() {
        getJPanel().removeAll();
        rulesPanel = null;
        getJPanel().add(getRulesPanel(), BorderLayout.NORTH);
        this.validate();
        this.repaint();
    }

    public class ExitAction extends AbstractAction {

        private static final long serialVersionUID = -8058749276509227718L;

        public void actionPerformed(ActionEvent ev) {
            setVisible(false);
        }
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