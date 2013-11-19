package se.cambio.cds.gdl.editor.view.listeners;

import org.apache.log4j.Logger;
import se.cambio.cds.gdl.editor.view.applicationobjects.ReadableRuleLineFactory;
import se.cambio.cds.gdl.editor.view.applicationobjects.RuleLineDirectory;
import se.cambio.cds.gdl.editor.view.panels.DraggableSelectableRuleLinePanel;
import se.cambio.cds.gdl.editor.view.panels.RuleLinesPanel;
import se.cambio.cds.gdl.editor.view.panels.rulelinecontainers.BaseRuleLineContainerPanel;
import se.cambio.cds.gdl.editor.view.panels.rulelinecontainers.MultipleRuleLinePanel;
import se.cambio.cds.gdl.editor.view.panels.rulelinecontainers.RuleLineContainerPanel;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public class SelectableRuleLineDragMouseListener extends MouseAdapter {

    private RuleLinesPanel _ruleLinesPanel = null;
    private RuleLineContainerPanel draggedPanel = null;
    private RuleLineContainerPanel backUpRLC = null;

    public SelectableRuleLineDragMouseListener(RuleLinesPanel ruleLinesPanel){
        _ruleLinesPanel = ruleLinesPanel;
    }

    @Override
    public void mousePressed(MouseEvent me) {
        RuleLine ruleLine = null;
        JComponent clickedJComponent = (JComponent)me.getSource();
        Point p = me.getPoint();
        p = SwingUtilities.convertPoint(clickedJComponent, p, _ruleLinesPanel);
        if (clickedJComponent instanceof DraggableSelectableRuleLinePanel){
            ruleLine = ((DraggableSelectableRuleLinePanel)clickedJComponent).getRuleLine();
            draggedPanel = ReadableRuleLineFactory.createRuleLineContainer(_ruleLinesPanel, ruleLine);
        }else{
            //Component component = _ruleLinesPanel.findComponentAt(p);
            draggedPanel = getRuleLineContainer(clickedJComponent);
            if (draggedPanel!=null){
                backUpRLC = getRuleLineContainer(draggedPanel.getParent());
                ruleLine = draggedPanel.getRuleLine();
                if (ruleLine.getParentRuleLine()!=null){
                    ruleLine.detachFromParent();
                }else{
                    _ruleLinesPanel.removeRuleLine(ruleLine);
                }
            }
        }
        if (draggedPanel!=null){
            _ruleLinesPanel.add(draggedPanel, JLayeredPane.DRAG_LAYER);
            Dimension d = draggedPanel.getPreferredSize();
            Rectangle r = new Rectangle(0,0, (int)d.getWidth(), (int)d.getHeight());
            draggedPanel.setBounds(r);
            draggedPanel.setLocation(p);
            _ruleLinesPanel.showCompatibility(ruleLine);
            if (me.getClickCount()>=2){
                clearRuleLineDraggableLayer(_ruleLinesPanel);
                addToComponent(_ruleLinesPanel.getBaseRuleLinePanel());
            }
        }
    }

    @Override
    public void mouseDragged(MouseEvent me) {
        if (draggedPanel == null) {
            return;
        }
        JComponent clickedJComponent = (JComponent)me.getSource();
        Point p = me.getPoint();
        p = SwingUtilities.convertPoint(clickedJComponent, p, _ruleLinesPanel);
        draggedPanel.setLocation(p);
        if (p.getY()>_ruleLinesPanel.getHeight()){
            JScrollBar scrollBar = _ruleLinesPanel.getRuleLinesJScrollPane().getVerticalScrollBar();
            scrollBar.setValue(scrollBar.getValue()+3);
        }else if (p.getY()<0){
            JScrollBar scrollBar = _ruleLinesPanel.getRuleLinesJScrollPane().getVerticalScrollBar();
            scrollBar.setValue(scrollBar.getValue()-3);
        }
    }

    @Override
    public void mouseReleased(MouseEvent me) {
        if (draggedPanel == null) {
            return;
        }
        JComponent clickedJComponent = (JComponent)me.getSource();
        Point p = me.getPoint();
        p = SwingUtilities.convertPoint(clickedJComponent, p, _ruleLinesPanel);
        clearRuleLineDraggableLayer(_ruleLinesPanel);
        Component comp = _ruleLinesPanel.findComponentAt(p);
        addToComponent(comp);
    }

    private void addToComponent(Component comp){
        RuleLineContainerPanel ruleLineContainer = getRuleLineContainer(comp);
        boolean inserted = false;
        while (ruleLineContainer!=null){
            if (ruleLineContainer instanceof MultipleRuleLinePanel){
                MultipleRuleLinePanel multipleRuleLinePanel = (MultipleRuleLinePanel) ruleLineContainer;
                RuleLine ruleLineAux = multipleRuleLinePanel.getRuleLine();
                if (RuleLineDirectory.checkRuleLineCompatibility(draggedPanel.getRuleLine(), ruleLineAux)){
                    multipleRuleLinePanel.addRuleLine(draggedPanel.getRuleLine());
                    Logger.getLogger(this.getClass()).info("Add into "+MultipleRuleLinePanel.class.getName());
                    inserted = true;
                    break;
                }
            }else if (ruleLineContainer instanceof BaseRuleLineContainerPanel){
                if (RuleLineDirectory.checkRuleLineCompatibility(draggedPanel.getRuleLine(), null)){
                    _ruleLinesPanel.addRuleLine(draggedPanel.getRuleLine());
                    Logger.getLogger(this.getClass()).info("Add into "+BaseRuleLineContainerPanel.class.getName());
                    inserted = true;
                    break;
                }
            }
            ruleLineContainer = getRuleLineContainer(ruleLineContainer.getParent());
        }
        if (!inserted && backUpRLC!=null){
            if (backUpRLC.getRuleLine()!=null){
                backUpRLC.getRuleLine().addChildRuleLine(draggedPanel.getRuleLine());
            }else{
                _ruleLinesPanel.addRuleLine(draggedPanel.getRuleLine());
            }
        }
        _ruleLinesPanel.refresh();
        backUpRLC = null;
        draggedPanel = null;
    }

    private static void clearRuleLineDraggableLayer(RuleLinesPanel ruleLinesPanel){
        for (Component comp : ruleLinesPanel.getComponentsInLayer(JLayeredPane.DRAG_LAYER)) {
            ruleLinesPanel.remove(comp);
        }
        ruleLinesPanel.repaint();
    }

    public static RuleLineContainerPanel getRuleLineContainer(Component component){
        while (component!=null && !(component instanceof RuleLineContainerPanel)){
            component = component.getParent();
        }
        if (component instanceof RuleLineContainerPanel){
            return (RuleLineContainerPanel)component;
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