package se.cambio.cds.gdl.editor.view.panels;

import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.JXPathNotFoundException;
import se.cambio.cds.formgen.view.util.SpringUtilities;
import se.cambio.cds.gdl.editor.controller.interfaces.EditorController;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;

public class DescriptionPanel extends JPanel {

    private static final long serialVersionUID = 1L;

    private EditorController controller = null;

    private JSplitPane mainSplitPane;
    private JScrollPane leftPanel;
    private JPanel detailsPanel;
    private JPanel authorDetailsPanel;
    private JXPathContext descriptionContext = null;
    private JXPathContext conceptContext = null;
    private JPanel authorshipLifecyclePanel;
    private ListPanel contributorsPanel;
    private ListPanel keywordsPanel;
    private JPanel purposePanel;
    private JPanel usePanel;
    private JPanel misusePanel;
    private JPanel references;
    private JPanel conceptPanel;
    private JPanel descriptionPanel;
    private Window window;

    DescriptionPanel(EditorController controller, Window window) {
        this.controller = controller;
        this.window = window;
        descriptionContext = JXPathContext.newContext(this.controller.getResourceDescription());
        conceptContext = JXPathContext.newContext(this.controller.getConceptTerm());
        init();
    }

    public void init() {
        this.setLayout(new BorderLayout());
        this.add(getMainSplitPane());
    }

    private JSplitPane getMainSplitPane() {
        if (mainSplitPane == null) {
            mainSplitPane = new JSplitPane();
            mainSplitPane.setOrientation(javax.swing.JSplitPane.HORIZONTAL_SPLIT);
            mainSplitPane.setResizeWeight(0.1);
            mainSplitPane.setLeftComponent(getLeftPanel());
            mainSplitPane.setRightComponent(getDetailsPanel());
        }
        return mainSplitPane;
    }


    private JScrollPane getLeftPanel() {
        if (leftPanel == null) {
            leftPanel = new JScrollPane();
            JPanel aux = new JPanel();
            aux.setLayout(new BoxLayout(aux, BoxLayout.Y_AXIS));
            leftPanel.setViewportView(aux);
            aux.add(getConceptPanel());
            aux.add(getAuthorDetailsPanel());
            aux.add(getAuthorshipLifeCycle());
            aux.add(getKeywordsPanel());
            aux.add(getContributorsPanel());
        }
        return leftPanel;
    }

    private JPanel getConceptPanel() {
        if (conceptPanel == null) {
            conceptPanel = new JPanel(new SpringLayout());
            conceptPanel.add(new JLabel(GDLEditorLanguageManager.getMessage("Name") + ":"));
            JTextField nameTF = new JTextField();
            connect(conceptContext, "/text", nameTF);
            conceptPanel.add(nameTF);
            SpringUtilities.makeCompactGrid(conceptPanel,
                    1, 2,    //rows, cols
                    6, 6,   //initX, initY
                    6, 6);
        }
        return conceptPanel;
    }

    private JPanel getAuthorDetailsPanel() {
        if (authorDetailsPanel == null) {
            authorDetailsPanel = new JPanel(new SpringLayout());
            authorDetailsPanel.setBorder(BorderFactory.createTitledBorder(GDLEditorLanguageManager.getMessage("AuthorDetails")));
            authorDetailsPanel.add(new JLabel(GDLEditorLanguageManager.getMessage("AuthorName") + ":"));
            JTextField nameTF = new JTextField();
            connect(descriptionContext, "/originalAuthor/name", nameTF);
            authorDetailsPanel.add(nameTF);
            authorDetailsPanel.add(new JLabel(GDLEditorLanguageManager.getMessage("Email") + ":"));
            JTextField emailTF = new JTextField();
            connect(descriptionContext, "/originalAuthor/email", emailTF);
            authorDetailsPanel.add(emailTF);
            authorDetailsPanel.add(new JLabel(GDLEditorLanguageManager.getMessage("Organisation") + ":"));
            JTextField orgTF = new JTextField();
            connect(descriptionContext, "/originalAuthor/organisation", orgTF);
            authorDetailsPanel.add(orgTF);
            authorDetailsPanel.add(new JLabel(GDLEditorLanguageManager.getMessage("Date") + ":"));
            JTextField dateTF = new JTextField();
            connect(descriptionContext, "/originalAuthor/date", dateTF);
            authorDetailsPanel.add(dateTF);
            SpringUtilities.makeCompactGrid(authorDetailsPanel,
                    4, 2,    //rows, cols
                    6, 6,   //initX, initY
                    6, 6);
        }
        return authorDetailsPanel;
    }

    private JPanel getAuthorshipLifeCycle() {
        if (authorshipLifecyclePanel == null) {
            authorshipLifecyclePanel = new JPanel(new SpringLayout());
            authorshipLifecyclePanel.add(new JLabel(GDLEditorLanguageManager.getMessage("AuthorshipLifecycle") + ":"));
            JComboBox<String> lyfeCB = new JComboBox<>();
            lyfeCB.addItem("Not set");
            lyfeCB.addItem("Initial");
            lyfeCB.addItem("Author draft");
            lyfeCB.addItem("Committee draft");
            lyfeCB.addItem("Organisation draft");
            lyfeCB.addItem("Submitted");
            lyfeCB.addItem("Candidate");
            lyfeCB.addItem("Approved candidate");
            lyfeCB.addItem("Published");
            lyfeCB.addItem("Rejected");
            lyfeCB.addItem("Obsolete");
            lyfeCB.setSelectedIndex(0);
            connect(descriptionContext, "/lifecycleState", lyfeCB);
            authorshipLifecyclePanel.add(lyfeCB);
            authorshipLifecyclePanel.add(new JLabel(GDLEditorLanguageManager.getMessage("Copyright") + ":"));
            JTextField copyrightTF = new JTextField();
            String lang = controller.getCurrentLanguageCode();
            connect(descriptionContext, "/details/" + lang + "/copyright", copyrightTF);
            authorshipLifecyclePanel.add(copyrightTF);

            SpringUtilities.makeCompactGrid(authorshipLifecyclePanel,
                    2, 2,    //rows, cols
                    6, 6,   //initX, initY
                    6, 6);
        }
        return authorshipLifecyclePanel;
    }

    private ListPanel getKeywordsPanel() {
        if (keywordsPanel == null) {
            String lang = controller.getCurrentLanguageCode();
            keywordsPanel = new ListPanel(GDLEditorLanguageManager.getMessage("Keywords"), "/details/" + lang + "/keywords", descriptionContext, window);
        }
        return keywordsPanel;
    }

    private ListPanel getContributorsPanel() {
        if (contributorsPanel == null) {
            contributorsPanel = new ListPanel(GDLEditorLanguageManager.getMessage("Contributors"), "/otherContributors", descriptionContext, window);
        }
        return contributorsPanel;
    }


    private JPanel getDetailsPanel() {
        if (detailsPanel == null) {
            detailsPanel = new JPanel(new GridLayout(5, 1));
            JScrollPane aux = new JScrollPane();
            aux.setBorder(BorderFactory.createTitledBorder(GDLEditorLanguageManager.getMessage("Description")));
            aux.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
            aux.setViewportView(getDescriptionPanel());
            detailsPanel.add(aux);

            aux = new JScrollPane();
            aux.setBorder(BorderFactory.createTitledBorder(GDLEditorLanguageManager.getMessage("Purpose")));
            aux.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
            aux.setViewportView(getPurposePanel());
            detailsPanel.add(aux);

            aux = new JScrollPane();
            aux.setBorder(BorderFactory.createTitledBorder(GDLEditorLanguageManager.getMessage("Use")));
            aux.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
            aux.setViewportView(getUsePanel());
            detailsPanel.add(aux);

            aux = new JScrollPane();
            aux.setBorder(BorderFactory.createTitledBorder(GDLEditorLanguageManager.getMessage("Misuse")));
            aux.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
            aux.setViewportView(getMisusePanel());
            detailsPanel.add(aux);

            aux = new JScrollPane();
            aux.setBorder(BorderFactory.createTitledBorder(GDLEditorLanguageManager.getMessage("References")));
            aux.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
            aux.setViewportView(getReferencePanel());
            detailsPanel.add(aux);
        }
        return detailsPanel;
    }

    private JPanel getDescriptionPanel() {
        if (descriptionPanel == null) {
            descriptionPanel = new JPanel(new BorderLayout());
            JTextArea ta = new JTextArea();
            ta.setLineWrap(true);
            ta.setWrapStyleWord(true);
            connect(conceptContext, "/description", ta);
            ta.setBorder(BorderFactory.createEtchedBorder());
            ta.setCaretPosition(0);
            descriptionPanel.add(ta, BorderLayout.CENTER);
        }
        return descriptionPanel;
    }

    private JPanel getPurposePanel() {
        if (purposePanel == null) {
            purposePanel = new JPanel(new BorderLayout());
            JTextArea ta = new JTextArea();
            ta.setLineWrap(true);
            ta.setWrapStyleWord(true);
            String lang = controller.getCurrentLanguageCode();
            connect(descriptionContext, "/details/" + lang + "/purpose", ta);
            ta.setBorder(BorderFactory.createEtchedBorder());
            ta.setCaretPosition(0);
            purposePanel.add(ta, BorderLayout.CENTER);
        }
        return purposePanel;
    }

    private JPanel getUsePanel() {
        if (usePanel == null) {
            usePanel = new JPanel(new BorderLayout());
            JTextArea ta = new JTextArea();
            ta.setLineWrap(true);
            ta.setWrapStyleWord(true);
            String lang = controller.getCurrentLanguageCode();
            connect(descriptionContext, "/details/" + lang + "/use", ta);
            ta.setBorder(BorderFactory.createEtchedBorder());
            ta.setCaretPosition(0);
            usePanel.add(ta, BorderLayout.CENTER);
        }
        return usePanel;
    }


    private JPanel getMisusePanel() {
        if (misusePanel == null) {
            misusePanel = new JPanel(new BorderLayout());
            JTextArea ta = new JTextArea();
            ta.setLineWrap(true);
            ta.setWrapStyleWord(true);
            String lang = controller.getCurrentLanguageCode();
            connect(descriptionContext, "/details/" + lang + "/misuse", ta);
            ta.setBorder(BorderFactory.createEtchedBorder());
            ta.setCaretPosition(0);
            misusePanel.add(ta, BorderLayout.CENTER);
        }
        return misusePanel;
    }

    private JPanel getReferencePanel() {
        if (references == null) {
            references = new JPanel(new BorderLayout());
            JTextArea ta = new JTextArea();
            ta.setLineWrap(true);
            ta.setWrapStyleWord(true);
            connect(descriptionContext, "/otherDetails/references", ta);
            ta.setBorder(BorderFactory.createEtchedBorder());
            ta.setCaretPosition(0);
            references.add(ta, BorderLayout.CENTER);
        }
        return references;
    }

    private class ComponentFocusAdapter extends FocusAdapter {
        private String path = null;
        private JXPathContext context = null;

        ComponentFocusAdapter(JXPathContext context, String path) {
            this.path = path;
            this.context = context;
        }

        public void focusLost(FocusEvent ev) {
            if (ev.getSource() instanceof JTextComponent) {
                JTextComponent textComponent = (JTextComponent) ev.getSource();
                saveText(context, path, textComponent);
            } else if (ev.getSource() instanceof JComboBox) {
                JComboBox comboBox = (JComboBox) ev.getSource();
                String text = (String) comboBox.getSelectedItem();
                context.setValue(path, text);
            }
        }


    }

    private void saveText(JXPathContext context, String xpath, JTextComponent textComponent) {
        String text = textComponent.getText();
        if (text != null) {
            text = text.replace("\"", "\\\"");
        }
        context.setValue(xpath, text);
    }

    private void connect(JXPathContext context, String xpath, JComponent component) {
        if (component instanceof JTextComponent) {
            JTextComponent textComponent = (JTextComponent) component;
            textComponent.getDocument().addDocumentListener(new DescriptionDocumentListener(context, xpath, textComponent));
        } else {
            component.addFocusListener(new ComponentFocusAdapter(context, xpath));
        }
        String value = null;
        try {
            value = (String) context.getValue(xpath);
        } catch (JXPathNotFoundException ex) {
            //Value not found
        }
        if (value != null) {
            if (component instanceof JTextComponent) {
                JTextComponent textComponent = (JTextComponent) component;
                value = value.replace("\\\"", "\"");
                textComponent.setText(value);
            } else if (component instanceof JComboBox) {
                ((JComboBox) component).setSelectedItem(value);
            }
        }
    }

    private class DescriptionDocumentListener implements DocumentListener {
        private JXPathContext context;
        private String xpath = null;
        private JTextComponent textComponent;

        DescriptionDocumentListener(JXPathContext context, String xpath, JTextComponent textComponent) {
            this.xpath = xpath;
            this.context = context;
            this.textComponent = textComponent;
        }

        @Override
        public void insertUpdate(DocumentEvent ev) {
            update();
        }

        @Override
        public void removeUpdate(DocumentEvent ev) {
            update();
        }

        @Override
        public void changedUpdate(DocumentEvent ev) {
            update();
        }

        private void update() {
            saveText(context, xpath, textComponent);
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