package se.cambio.cds.gdl.graph;

import se.cambio.cds.view.swing.panel.AbstractPluginPanel;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import java.awt.*;
import java.net.URL;

public class DependencyGraphGDLEditorPluginPanel extends AbstractPluginPanel {

    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private JScrollPane mainScrollPanel;
    private JComponent graph;

    public DependencyGraphGDLEditorPluginPanel(){
        init();
    }

    public void init(){
        this.setLayout(new BorderLayout());
        this.setFocusable(true);
    }

    private JScrollPane getMainScrollPanel(){
        if (mainScrollPanel==null){
            mainScrollPanel = new JScrollPane(getGraphComponent());
        }
        return mainScrollPanel;
    }

    private JComponent getGraphComponent(){
        if (graph ==null){
            graph = new JPanel();
        }
        return graph;
    }



    public void refresh(){
        if (mainScrollPanel!=null){
            this.remove(mainScrollPanel);
            mainScrollPanel = null;
            graph = null;
        }
        try {
            GDLDependencyGraph gdlDependencyGraph = new GDLDependencyGraph(getGuide());
            graph = gdlDependencyGraph.getGraph(getLanguage());
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e); //TODO
        }
        this.add(getMainScrollPanel());
        this.repaint();
        this.revalidate();
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                getMainScrollPanel().getVerticalScrollBar().setValue(0);
            }
        });
    }

    @Override
    public String getPluginName() {
        return "Dependency graph"; //TODO I18n
    }

    @Override
    public String getPluginDescription() {
        return "Dependency graph"; //TODO I18n
    }

    @Override
    public ImageIcon getPluginIcon() {
        URL url = DependencyGraphGDLEditorPluginPanel.class.getResource("/img/graph.png");
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