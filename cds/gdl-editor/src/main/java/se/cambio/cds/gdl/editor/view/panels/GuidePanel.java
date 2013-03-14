package se.cambio.cds.gdl.editor.view.panels;

import java.awt.BorderLayout;
import java.awt.Component;

import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.controller.exportplugins.GuideExportPlugin;
import se.cambio.cds.gdl.editor.controller.exportplugins.GuideExportPluginDirectory;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.LanguageManager;
import se.cambio.cds.gdl.editor.view.applicationobjects.RuleLineDirectory;
import se.cambio.cds.gdl.editor.view.panels.interfaces.RefreshablePanel;

public class GuidePanel extends JPanel {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private GDLEditor _controller;
    private JTabbedPane guideEditorTabPane = null;
    private JPanel descriptionPanel;
    private RulesPanel rulesPanel;
    private RuleLinesPanel preconditionsPanel;
    private RuleLinesPanel definitionsPanel;
    private TerminologyPanel ontologyPanel;
    private BindingsPanel bindingTabPanel;
    private GDLPanel gdlPanel;
    private HTMLPanel htmlPanel;

    public GuidePanel(GDLEditor controller){
	_controller = controller;
	init();
    }

    /**
     * This method initializes this
     */
    private  void init() {
	this.setLayout(new BorderLayout());
	this.add(getGuideEditorTabPane());
    }

    public JTabbedPane getGuideEditorTabPane(){
	if ( guideEditorTabPane == null){
	    guideEditorTabPane = new JTabbedPane();
	    guideEditorTabPane.addTab(
		    LanguageManager.getMessage("Description"), 
		    GDLEditorImageUtil.DESCRIPTION_ICON, 
		    getDescriptionPanel());
	    guideEditorTabPane.addTab(
		    LanguageManager.getMessage("Definitions"), 
		    GDLEditorImageUtil.SOURCE_ICON, 
		    getDefinitionsPanel());
	    guideEditorTabPane.addTab(
		    LanguageManager.getMessage("RuleList"), 
		    GDLEditorImageUtil.RULE_ICON, 
		    getRulesPanel());
	    guideEditorTabPane.addTab(
		    LanguageManager.getMessage("Preconditions"), 
		    GDLEditorImageUtil.CONDITION_ICON,
		    getPreconditionsPanel());
	    guideEditorTabPane.addTab(
		    LanguageManager.getMessage("Terminology"), 
		    GDLEditorImageUtil.TRANSLATE_ICON,
		    getTerminologyPanel());
	    guideEditorTabPane.addTab(
		    LanguageManager.getMessage("Binding"), 
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
	    
	    for (GuideExportPlugin guideExportPlugin : GuideExportPluginDirectory.getGuideExportPlugins()) {
		guideEditorTabPane.addTab(
			guideExportPlugin.getPluginName(),
			GDLEditorImageUtil.CONNECT_ICON,
			new GuideExportPluginPanel(_controller, guideExportPlugin)
			);
	    }

	    guideEditorTabPane.addChangeListener(new ChangeListener() {
		public void stateChanged(ChangeEvent e) {
		    if (e.getSource() instanceof JTabbedPane){
			Component comp = ((JTabbedPane)e.getSource()).getSelectedComponent();
			if (comp instanceof RefreshablePanel){
			    ((RefreshablePanel)comp).refresh();
			}
		    }
		}
	    });
	}
	return guideEditorTabPane;
    }

    private JPanel getDescriptionPanel(){
	if (descriptionPanel==null){
	    descriptionPanel = new DescriptionPanel(_controller);
	}
	return descriptionPanel;
    }

    public RulesPanel getRulesPanel(){
	if (rulesPanel==null){
	    rulesPanel = new RulesPanel(_controller);
	}
	return rulesPanel;
    }

    private RuleLinesPanel getPreconditionsPanel(){
	if (preconditionsPanel==null){
	    preconditionsPanel = 
		    new RuleLinesPanel(
			    _controller, 
			    RuleLineDirectory.getSelectableConditions(),
			    _controller.getPreconditionRuleLines(),
			    LanguageManager.getMessage("Preconditions"));
	}
	return preconditionsPanel;
    }

    private RuleLinesPanel getDefinitionsPanel(){
	if (definitionsPanel==null){
	    definitionsPanel = new RuleLinesPanel(
		    _controller, 
		    RuleLineDirectory.getSelectableDefinitions(),
		    _controller.getDefinitionRuleLines(),
		    LanguageManager.getMessage("Definitions"));
	}
	return definitionsPanel;
    }

    private TerminologyPanel getTerminologyPanel(){
	if (ontologyPanel==null){
	    ontologyPanel = new TerminologyPanel(_controller);
	}
	return ontologyPanel;
    }
    
    public BindingsPanel getBindingPanel(){
    	if (bindingTabPanel==null){
    	    bindingTabPanel = new BindingsPanel(_controller);
    	}
    	return bindingTabPanel;
        }
    
    private GDLPanel getGDLPanel(){
	if (gdlPanel==null){
	    gdlPanel = new GDLPanel(_controller);
	}
	return gdlPanel;
    }
    
    private HTMLPanel getHTMLPanel(){
	if (htmlPanel==null){
	    htmlPanel = new HTMLPanel(_controller);
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