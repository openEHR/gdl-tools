package se.cambio.cds.gdl.graph;

import se.cambio.cds.view.swing.panel.AbstractPluginPanel;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;
import java.net.URL;

public class GDLGraphPluginPanel extends AbstractPluginPanel {

    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private JTabbedPane tabbedPane;
    private DependencyGraphGDLEditorPluginPanel _dependencyGraphGDLEditorPluginPanel;
    private DecisionGraphGDLEditorPluginPanel _decisionGraphGDLEditorPluginPanel;

    public GDLGraphPluginPanel(){
        init();
    }

    public void init(){
        this.setLayout(new BorderLayout());
        this.setFocusable(true);
        this.add(getJTabbedPane());
    }

    private JTabbedPane getJTabbedPane(){
        if (tabbedPane==null){
            tabbedPane = new JTabbedPane();
            tabbedPane.addTab(
                    getDecisionGraphGDLEditorPluginPanel().getPluginName(),
                    getDecisionGraphGDLEditorPluginPanel().getPluginIcon(),
                    getDecisionGraphGDLEditorPluginPanel());
            tabbedPane.addTab(
                    getDependencyGraphGDLEditorPluginPanel().getPluginName(),
                    getDependencyGraphGDLEditorPluginPanel().getPluginIcon(),
                    getDependencyGraphGDLEditorPluginPanel());
            tabbedPane.addChangeListener(new ChangeListener() {
                @Override
                public void stateChanged(ChangeEvent e) {
                    refresh();
                }
            });
        }
        return tabbedPane;
    }

    private DecisionGraphGDLEditorPluginPanel getDecisionGraphGDLEditorPluginPanel(){
        if (_decisionGraphGDLEditorPluginPanel==null){
            _decisionGraphGDLEditorPluginPanel = new DecisionGraphGDLEditorPluginPanel();
        }
        return _decisionGraphGDLEditorPluginPanel;
    }

    private DependencyGraphGDLEditorPluginPanel getDependencyGraphGDLEditorPluginPanel(){
        if (_dependencyGraphGDLEditorPluginPanel==null){
            _dependencyGraphGDLEditorPluginPanel = new DependencyGraphGDLEditorPluginPanel();

        }
        return _dependencyGraphGDLEditorPluginPanel;
    }

    public void refresh(){
        Component comp = getJTabbedPane().getSelectedComponent();
        if (comp instanceof AbstractPluginPanel){
            AbstractPluginPanel pluginPanel = (AbstractPluginPanel)comp;
            pluginPanel.setGuide(getGuide());
            pluginPanel.setLanguage(getLanguage());
            pluginPanel.refresh();
        }
    }

    @Override
    public String getPluginName() {
        return "Graphs"; //TODO I18n
    }

    @Override
    public String getPluginDescription() {
        return "Graphs"; //TODO I18n
    }

    @Override
    public ImageIcon getPluginIcon() {
        URL url = GDLGraphPluginPanel.class.getResource("/img/graph.png");
        if (url != null)  {
            return new ImageIcon(url);
        }
        return null;
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