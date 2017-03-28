package se.cambio.cds.gdl.editor.view.panels;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.controller.panelplugins.GDLEditorPluginPanelManager;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.util.export.html.GuideHTMLExporter;

import javax.swing.*;
import java.awt.*;

public class GuidePanel extends JPanel {

    private static final long serialVersionUID = 1L;
    private GDLEditor controller;
    private JTabbedPane guideEditorTabPane = null;
    private JPanel descriptionPanel;
    private RulesPanel rulesPanel;
    private RuleLinesPanel preconditionsPanel;
    private RuleLinesPanel defaultActionsPanel;
    private RuleLinesPanel definitionsPanel;
    private TerminologyPanel terminologyPanel;
    private MultipleBindingsPanel bindingTabPanel;
    private GDLPanel gdlPanel;
    private HTMLPanel htmlPanel;
    private GuideHTMLExporter guideHtmlExporter;

    GuidePanel(GDLEditor controller, GuideHTMLExporter guideHtmlExporter) {
        this.controller = controller;
        this.guideHtmlExporter = guideHtmlExporter;
        init();
    }

    private void init() {
        this.setLayout(new BorderLayout());
        this.add(getGuideEditorTabPane());
    }

    public JTabbedPane getGuideEditorTabPane() {
        if (guideEditorTabPane == null) {
            guideEditorTabPane = new JTabbedPane();
            guideEditorTabPane.addTab(
                    GDLEditorLanguageManager.getMessage("Description"),
                    GDLEditorImageUtil.DESCRIPTION_ICON,
                    getRefreshableDescriptionPanel());
            guideEditorTabPane.addTab(
                    GDLEditorLanguageManager.getMessage("Definitions"),
                    GDLEditorImageUtil.SOURCE_ICON,
                    getDefinitionsPanel());
            guideEditorTabPane.addTab(
                    GDLEditorLanguageManager.getMessage("RuleList"),
                    GDLEditorImageUtil.RULE_ICON,
                    getRulesPanel());
            guideEditorTabPane.addTab(
                    GDLEditorLanguageManager.getMessage("Preconditions"),
                    GDLEditorImageUtil.CONDITION_ICON,
                    getPreconditionsPanel());
            guideEditorTabPane.addTab(
                    GDLEditorLanguageManager.getMessage("Defaults"),
                    GDLEditorImageUtil.ACTION_ICON,
                    getDefaultActionsPanel());
            guideEditorTabPane.addTab(
                    GDLEditorLanguageManager.getMessage("Terminology"),
                    GDLEditorImageUtil.TRANSLATE_ICON,
                    getTerminologyPanel());
            guideEditorTabPane.addTab(
                    GDLEditorLanguageManager.getMessage("Binding"),
                    GDLEditorImageUtil.ONTOLOGY_ICON,
                    getBindingPanel());
            guideEditorTabPane.addTab(
                    "GDL",
                    GDLEditorImageUtil.GDL_LANG_ICON,
                    getGDLPanel());
            guideEditorTabPane.addTab(
                    "HTML",
                    GDLEditorImageUtil.HTML_ICON,
                    getHTMLPanel());
            guideEditorTabPane.setFocusable(true);
            for (AbstractPluginPanel abstractPluginPanel : GDLEditorPluginPanelManager.getPluginPanels()) {
                abstractPluginPanel.setGdlEditor(controller);
                guideEditorTabPane.addTab(
                        abstractPluginPanel.getPluginName(),
                        abstractPluginPanel.getPluginIcon(),
                        abstractPluginPanel
                );
            }

            guideEditorTabPane.addChangeListener(e -> {
                if (e.getSource() instanceof JTabbedPane) {
                    final Component comp = ((JTabbedPane) e.getSource()).getSelectedComponent();
                    controller.tabChanged(comp);
                }
            });
        }
        return guideEditorTabPane;
    }

    private JPanel getRefreshableDescriptionPanel() {
        if (descriptionPanel == null) {
            descriptionPanel = new RefreshableDescriptionPanel(controller, controller.getEditorWindow());
        }
        return descriptionPanel;
    }

    RulesPanel getRulesPanel() {
        if (rulesPanel == null) {
            rulesPanel = new RulesPanel(controller);
        }
        return rulesPanel;
    }

    private RuleLinesPanel getPreconditionsPanel() {
        if (preconditionsPanel == null) {
            preconditionsPanel = new PreconditionRuleLinesPanel(controller);
        }
        return preconditionsPanel;
    }

    private RuleLinesPanel getDefaultActionsPanel() {
        if (defaultActionsPanel == null) {
            defaultActionsPanel = new DefaultActionsPanel(controller);
        }
        return defaultActionsPanel;
    }

    private RuleLinesPanel getDefinitionsPanel() {
        if (definitionsPanel == null) {
            definitionsPanel = new DefinitionRuleLinesPanel(controller);
        }
        return definitionsPanel;
    }

    private TerminologyPanel getTerminologyPanel() {
        if (terminologyPanel == null) {
            terminologyPanel = new TerminologyPanel(controller);
        }
        return terminologyPanel;
    }

    MultipleBindingsPanel getBindingPanel() {
        if (bindingTabPanel == null) {
            bindingTabPanel = new MultipleBindingsPanel(controller);
        }
        return bindingTabPanel;
    }

    private GDLPanel getGDLPanel() {
        if (gdlPanel == null) {
            gdlPanel = new GDLPanel(controller);
        }
        return gdlPanel;
    }

    private HTMLPanel getHTMLPanel() {
        if (htmlPanel == null) {
            htmlPanel = new HTMLPanel(controller, guideHtmlExporter);
        }
        return htmlPanel;
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