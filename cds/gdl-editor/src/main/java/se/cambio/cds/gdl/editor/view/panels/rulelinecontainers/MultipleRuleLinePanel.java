package se.cambio.cds.gdl.editor.view.panels.rulelinecontainers;

import se.cambio.cds.gdl.editor.controller.RuleLineCloner;
import se.cambio.cds.gdl.editor.view.applicationobjects.ReadableRuleLineFactory;
import se.cambio.cds.gdl.editor.view.applicationobjects.RuleLineDirectory;
import se.cambio.cds.gdl.editor.view.panels.RuleLinesPanel;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;

import javax.swing.*;
import java.awt.*;

public class MultipleRuleLinePanel extends RuleLineContainerPanel{

    /**
     *
     */
    private JPanel ruleLineListPanel = null;

    private static final long serialVersionUID = 1L;

    private RuleLinesPanel _ruleLinesPanel =null;
    private RuleLine _ruleLine = null;

    private JPanel mainPanel;

    public MultipleRuleLinePanel(RuleLinesPanel ruleLinesPanel, RuleLine ruleLine){
        _ruleLinesPanel = ruleLinesPanel;
        _ruleLine = ruleLine;
        init();
    }

    private void init(){
        this.setLayout(new FlowLayout(FlowLayout.LEFT,0,0));
        this.add(getMainPanel());
    }

    protected JPanel getMainPanel(){
        if (mainPanel==null){
            mainPanel = new JPanel(new BorderLayout(0,0));
            RuleLine ruleLineCheck = _ruleLinesPanel.getRuleLineCheck();
            if (ruleLineCheck!=null){
                if (RuleLineDirectory.checkRuleLineCompatibility(ruleLineCheck, _ruleLine)){
                    mainPanel.setBorder(BorderFactory.createEtchedBorder());
                }
            }
            JPanel aux = new JPanel(new BorderLayout(0,0));
            mainPanel.add(aux, BorderLayout.CENTER);
            aux.add(getRuleLineListPanel(), BorderLayout.NORTH);
            if (_ruleLine.getChildrenRuleLines().isEmpty()){
                getRuleLineListPanel().add(Box.createRigidArea(new Dimension(50,20)));
            }else{
                for (RuleLine ruleLine : _ruleLine.getChildrenRuleLines()) {
                    JPanel panel = ReadableRuleLineFactory.createRuleLineContainer(_ruleLinesPanel, ruleLine);
                    JPanel auxLine = new JPanel(new BorderLayout(0,0));
                    auxLine.add(Box.createHorizontalStrut(16), BorderLayout.WEST);
                    auxLine.add(panel, BorderLayout.CENTER);
                    getRuleLineListPanel().add(auxLine);
                }
            }
        }
        return mainPanel;
    }

    public void addRuleLine(RuleLine ruleLine){
        if (RuleLineDirectory.isDirectoryRuleLine(ruleLine)){
            ruleLine = RuleLineCloner.clone(ruleLine);
        }
        _ruleLine.addChildRuleLine(ruleLine);
        ruleLineAdded(ruleLine);
    }

    private JPanel getRuleLineListPanel(){
        if (ruleLineListPanel==null){
            ruleLineListPanel = new JPanel();
            ruleLineListPanel.setLayout(new BoxLayout(ruleLineListPanel, BoxLayout.Y_AXIS));
        }
        return ruleLineListPanel;
    }

    public RuleLine getRuleLine() {
        return _ruleLine;
    }

    public RuleLinesPanel getRuleLinesPanel() {
        return _ruleLinesPanel;
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