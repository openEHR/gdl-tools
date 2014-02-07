package se.cambio.cds.gdl.editor.view.panels;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.controller.RuleLineCloner;
import se.cambio.cds.gdl.editor.view.applicationobjects.RuleLineDirectory;
import se.cambio.cds.gdl.editor.view.listeners.SelectableRuleLineDragMouseListener;
import se.cambio.cds.gdl.editor.view.panels.interfaces.RefreshablePanel;
import se.cambio.cds.gdl.editor.view.panels.rulelinecontainers.BaseRuleLineContainerPanel;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.RuleLineElementWithValue;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.util.Collection;
import java.util.List;

public abstract class RuleLinesPanel extends JLayeredPane implements RefreshablePanel{

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private JPanel toolsPanel;

    private JPanel _selectionPanel = null;
    private GDLEditor _controller = null;
    private JScrollPane jScrollPaneCond;
    private Collection<RuleLine> _selectableRuleLines = null;
    private String _title = null;
    private JPanel mainPanel;
    private SelectableRuleLineDragMouseListener selectableRuleLineDragMouseListener;
    private BaseRuleLineContainerPanel _baseRuleLinePanel;
    private RuleLine _ruleLineCheck = null;

    public RuleLinesPanel(GDLEditor controller, Collection<RuleLine> selectableRuleLines,String title){
        _controller = controller;
        _selectableRuleLines = selectableRuleLines;
        _title = title;
        init();
    }

    private void init(){
        addComponentListener(new ComponentAdapter() {
            public void componentResized(ComponentEvent e) {
                getMainPanel().setBounds(0,0, getWidth(),getHeight());
                revalidate();
                repaint();
            }
        });
        this.add(getMainPanel(), JLayeredPane.DEFAULT_LAYER);
    }

    public JPanel getMainPanel(){
        if (mainPanel==null){
            mainPanel = new JPanel(new BorderLayout());
            mainPanel.add(getRuleLinesJScrollPane(), BorderLayout.CENTER);
            mainPanel.add(getToolsPanel(), BorderLayout.EAST);
        }
        return mainPanel;
    }


    public JPanel getToolsPanel(){
        if (toolsPanel==null){
            toolsPanel = new JPanel(new BorderLayout());
            toolsPanel.add(getSelectionPanel(), BorderLayout.NORTH);
        }
        return toolsPanel;
    }

    private JPanel getSelectionPanel(){
        if (_selectionPanel==null){
            _selectionPanel = new JPanel(new BorderLayout());
            _selectionPanel.setBorder(BorderFactory.createTitledBorder(_title));
            JPanel aux = new JPanel();
            aux.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
            aux.setLayout(new BoxLayout(aux, BoxLayout.Y_AXIS));
            _selectionPanel.add(aux, BorderLayout.NORTH);
            for (RuleLine ruleLine : _selectableRuleLines) {
                DraggableSelectableRuleLinePanel label = new DraggableSelectableRuleLinePanel(ruleLine);
                label.addMouseListener(getSelectableRuleLineDragMouseListener());
                label.addMouseMotionListener(getSelectableRuleLineDragMouseListener());
                aux.add(label);
            }
        }
        return _selectionPanel;
    }

    public SelectableRuleLineDragMouseListener getSelectableRuleLineDragMouseListener(){
        if (selectableRuleLineDragMouseListener==null){
            selectableRuleLineDragMouseListener =
                    new SelectableRuleLineDragMouseListener(this);
        }
        return selectableRuleLineDragMouseListener;
    }

    protected abstract List<RuleLine> getRuleLines();

    public void addRuleLine(RuleLine ruleLine){
        if (RuleLineDirectory.isDirectoryRuleLine(ruleLine)){
            ruleLine = RuleLineCloner.clone(ruleLine);
        }
        getRuleLines().add(0, ruleLine);
        ruleLineAdded(ruleLine);
    }

    public void ruleLineAdded(RuleLine ruleLine){
	/* AUTOMATIC DIALOG OPENER (disabled for now)
	RuleLineElementWithValue<?> rlewv = null;
	Iterator<RuleLineElement> i = ruleLine.getRuleLineElements().iterator();
	while(i.hasNext()){
	    RuleLineElement rle = i.next();
	    if (rle instanceof RuleLineElementWithValue<?>){
		rlewv = (RuleLineElementWithValue<?>)rle;
		break;
	    }
	}
	if (rlewv!=null && rlewv.getValue()==null){
	    RuleElementEditor.edit(rlewv);
	}*/
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
        if (_baseRuleLinePanel==null){
            _baseRuleLinePanel = new BaseRuleLineContainerPanel(this, getRuleLines());
            _baseRuleLinePanel.setBorder(BorderFactory.createTitledBorder(_title));
        }
        return _baseRuleLinePanel;
    }


    public void showCompatibility(RuleLine ruleLine){
        _ruleLineCheck = ruleLine;
        refresh();
        _ruleLineCheck = null;
    }

    public RuleLine getRuleLineCheck(){
        return _ruleLineCheck;
    }

    public void refresh(){
        _baseRuleLinePanel = null;
        getRuleLinesJScrollPane().setViewportView(getBaseRuleLinePanel());
    }

    public GDLEditor getController(){
        return _controller;
    }

    public String getDescription(RuleLineElementWithValue<?> ruleLineElementWithValue){
        if (ruleLineElementWithValue!=null){
            return ruleLineElementWithValue.getDescription();
        }else{
            return null;
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