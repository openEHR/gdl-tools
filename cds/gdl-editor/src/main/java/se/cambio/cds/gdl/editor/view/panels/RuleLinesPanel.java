package se.cambio.cds.gdl.editor.view.panels;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.controller.RuleLineCloner;
import se.cambio.cds.gdl.editor.view.applicationobjects.RuleLineDirectory;
import se.cambio.cds.gdl.editor.view.listeners.SelectableRuleLineDragMouseListener;
import se.cambio.cds.gdl.editor.view.panels.rulelinecontainers.BaseRuleLineContainerPanel;
import se.cambio.cds.gdl.model.readable.rule.RuleLineCollection;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.view.swing.panel.interfaces.RefreshablePanel;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.util.Collection;

public abstract class RuleLinesPanel extends JLayeredPane implements RefreshablePanel{

    private static final long serialVersionUID = 1L;

    private JPanel toolsPanel;

    private JPanel selectionPanel = null;
    private GDLEditor controller = null;
    private JScrollPane jScrollPaneCond;
    private Collection<RuleLine> selectableRuleLines = null;
    private String title = null;
    private JPanel mainPanel;
    private SelectableRuleLineDragMouseListener selectableRuleLineDragMouseListener;
    private BaseRuleLineContainerPanel baseRuleLinePanel;
    private RuleLine ruleLineCheck = null;

    public RuleLinesPanel(GDLEditor controller, Collection<RuleLine> selectableRuleLines,String title){
        this.controller = controller;
        this.selectableRuleLines = selectableRuleLines;
        this.title = title;
        init();
    }

    private void init(){
        addComponentListener(new ComponentAdapter() {
            public void componentResized(ComponentEvent ev) {
                getMainPanel().setBounds(0,0, getWidth(),getHeight());
                revalidate();
                repaint();
            }
        });
        this.add(getMainPanel(), JLayeredPane.DEFAULT_LAYER);
    }

    private JPanel getMainPanel(){
        if (mainPanel==null){
            mainPanel = new JPanel(new BorderLayout());
            mainPanel.add(getRuleLinesJScrollPane(), BorderLayout.CENTER);
            mainPanel.add(getToolsPanel(), BorderLayout.EAST);
        }
        return mainPanel;
    }


    private JPanel getToolsPanel(){
        if (toolsPanel==null){
            toolsPanel = new JPanel(new BorderLayout());
            toolsPanel.add(getSelectionPanel(), BorderLayout.NORTH);
        }
        return toolsPanel;
    }

    private JPanel getSelectionPanel(){
        if (selectionPanel ==null){
            selectionPanel = new JPanel(new BorderLayout());
            selectionPanel.setBorder(BorderFactory.createTitledBorder(title));
            JPanel aux = new JPanel();
            aux.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
            aux.setLayout(new BoxLayout(aux, BoxLayout.Y_AXIS));
            selectionPanel.add(aux, BorderLayout.NORTH);
            for (RuleLine ruleLine : selectableRuleLines) {
                DraggableSelectableRuleLinePanel label = new DraggableSelectableRuleLinePanel(ruleLine);
                label.addMouseListener(getSelectableRuleLineDragMouseListener());
                label.addMouseMotionListener(getSelectableRuleLineDragMouseListener());
                aux.add(label);
            }
        }
        return selectionPanel;
    }

    public SelectableRuleLineDragMouseListener getSelectableRuleLineDragMouseListener(){
        if (selectableRuleLineDragMouseListener==null){
            selectableRuleLineDragMouseListener =
                    new SelectableRuleLineDragMouseListener(this, controller);
        }
        return selectableRuleLineDragMouseListener;
    }

    protected abstract RuleLineCollection getRuleLines();

    public void addRuleLine(RuleLine ruleLine){
        if (RuleLineDirectory.isDirectoryRuleLine(ruleLine)){
            ruleLine = controller.cloneRuleLine(ruleLine);
        }
        getRuleLines().add(0, ruleLine);
    }

    public void removeRuleLine(RuleLine ruleLine){
        getRuleLines().remove(ruleLine);
    }

    public JScrollPane getRuleLinesJScrollPane() {
        if (jScrollPaneCond == null) {
            jScrollPaneCond = new JScrollPane();
            jScrollPaneCond.setViewportView(getBaseRuleLinePanel());
        }
        return jScrollPaneCond;
    }

    public BaseRuleLineContainerPanel getBaseRuleLinePanel(){
        if (baseRuleLinePanel ==null){
            baseRuleLinePanel = new BaseRuleLineContainerPanel(this, getRuleLines(), controller);
            baseRuleLinePanel.setBorder(BorderFactory.createTitledBorder(title));
        }
        return baseRuleLinePanel;
    }


    public void showCompatibility(RuleLine ruleLine){
        ruleLineCheck = ruleLine;
        refresh();
        ruleLineCheck = null;
    }

    public RuleLine getRuleLineCheck(){
        return ruleLineCheck;
    }

    public void refresh(){
        baseRuleLinePanel = null;
        getRuleLinesJScrollPane().setViewportView(getBaseRuleLinePanel());
    }

    public GDLEditor getController(){
        return controller;
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