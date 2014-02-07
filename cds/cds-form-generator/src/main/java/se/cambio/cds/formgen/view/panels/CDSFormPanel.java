package se.cambio.cds.formgen.view.panels;

import se.cambio.cds.formgen.controller.FormGeneratorController;
import se.cambio.cds.formgen.controller.sw.ExecuteRSW;
import se.cambio.cds.gdl.model.readable.ReadableGuide;
import se.cambio.cds.gdl.model.readable.rule.ReadableRule;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.facade.execution.vo.RuleReference;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.Domains;
import se.cambio.cds.view.swing.dialogs.DialogRuleExecutionList;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.view.util.JLinkLabel;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.*;
import java.util.List;

public class CDSFormPanel extends JPanel{

    private static final long serialVersionUID = 1L;

    private FormGeneratorController _formGenerator = null;
    private JPanel inputPanel;
    private JPanel resultPanel;
    private JLinkLabel executedRulesLabel;
    //private Map<String, JPanel> elementGroupPanelMap = null;
    private JPanel executionButtonPanel;
    private GridBagConstraints inputGBC;
    private GridBagConstraints resultGBC;
    private Collection<ElementInstanceGroupPanel> _eigps = null;

    public CDSFormPanel(FormGeneratorController formGenerator){
        this.setLayout(new BorderLayout());
        _formGenerator = formGenerator;
        _eigps = new ArrayList<ElementInstanceGroupPanel>();
        init();
    }

    private void init(){
        JPanel panelAux = new JPanel(new BorderLayout());
        panelAux.add(getInputPanel(), BorderLayout.CENTER);
        panelAux.add(getExecutionButtonPanel(), BorderLayout.SOUTH);
        this.add(panelAux, BorderLayout.NORTH);
        panelAux = new JPanel(new BorderLayout());
        panelAux.add(getResultPanel(), BorderLayout.NORTH);
        this.add(panelAux, BorderLayout.CENTER);
        this.repaint();
        this.revalidate();
    }

    public void setInputElements(Collection<ArchetypeReference> archetypeReferences){
        this.removeAll();
        inputPanel = null;
        _eigps.clear();
        if (archetypeReferences!=null){
            addArchetypeReferences(archetypeReferences, true);
        }
        init();
    }

    public void setResultElements(Collection<ArchetypeReference> archetypeReferences){
        this.removeAll();
        resultPanel = null;
        if (archetypeReferences!=null){
            addArchetypeReferences(archetypeReferences, false);
        }
        getResultPanel().setVisible(true);
        init();
    }

    private void addArchetypeReferences(Collection<ArchetypeReference> archetypeReferences, boolean input){
        for (ArchetypeReference ar : archetypeReferences) {
            ElementInstanceGroupPanel eigp = new ElementInstanceGroupPanel(ar, _formGenerator.getTermDefinition(), input);
            eigp.setAlignmentX(Component.LEFT_ALIGNMENT);
            if (input){
                getInputPanel().add(eigp);
                _eigps.add(eigp);
            }else{
                getResultPanel().add(eigp);
            }
        }
    }

    public Collection<ArchetypeReference> getAllArchetypeReferences(Collection<ElementInstance> elementInstances){
        Set<ArchetypeReference> archetypeReferences = new HashSet<ArchetypeReference>();
        for (ElementInstance elementInstance : elementInstances) {
            archetypeReferences.add(elementInstance.getArchetypeReference());
        }
        return archetypeReferences;
    }

    public Collection<ArchetypeReference> getCDSArchetypeReferences(Collection<ElementInstance> elementInstances){
        Set<ArchetypeReference> archetypeReferences = new HashSet<ArchetypeReference>();
        for (ElementInstance elementInstance : elementInstances) {
            ArchetypeReference ar = elementInstance.getArchetypeReference();
            if (Domains.CDS_ID.equals(ar.getIdDomain())){
                archetypeReferences.add(ar);
            }
        }
        return archetypeReferences;
    }

    public Collection<ElementInstance> getElementInstances(){
        Collection<ElementInstance> elementInstances = new ArrayList<ElementInstance>();
        for (ElementInstanceGroupPanel eigp : _eigps) {
            elementInstances.addAll(eigp.getElementInstances());
        }
        return elementInstances;
    }

    public JPanel getInputPanel(){
        if (inputPanel==null){
            inputPanel = new JPanel();
            inputPanel.setLayout(new BoxLayout(inputPanel, BoxLayout.Y_AXIS));
            inputPanel.setBorder(BorderFactory.createTitledBorder(OpenEHRLanguageManager.getMessage("Input")));
        }
        return  inputPanel;
    }

    public GridBagConstraints getInputGBC(){
        if (inputGBC==null){
            inputGBC = new GridBagConstraints();
            inputGBC.gridx = 0;
            inputGBC.gridy = 0;
            inputGBC.anchor = GridBagConstraints.WEST;
            inputGBC.fill= GridBagConstraints.BOTH;
        }
        return inputGBC;
    }

    public GridBagConstraints getResulGBC(){
        if (resultGBC==null){
            resultGBC = new GridBagConstraints();
            resultGBC.gridx = 0;
            resultGBC.gridy = 0;
            resultGBC.anchor = GridBagConstraints.WEST;
            resultGBC.fill= GridBagConstraints.BOTH;
        }
        return resultGBC;
    }

    public JPanel getResultPanel(){
        if (resultPanel==null){
            resultPanel = new JPanel();
            resultPanel.setLayout(new BoxLayout(resultPanel, BoxLayout.Y_AXIS));
            resultPanel.setBorder(BorderFactory.createTitledBorder(OpenEHRLanguageManager.getMessage("Result")));
            resultPanel.setVisible(false);
        }
        return  resultPanel;
    }

    private JPanel getExecutionButtonPanel(){
        if (executionButtonPanel==null){
            executionButtonPanel = new JPanel();
            JButton jButton = new JButton(OpenEHRLanguageManager.getMessage("Execute"));
            jButton.setIcon(OpenEHRImageUtil.LIGHTNING_ICON);
            jButton.addActionListener(new ExecutionActionListener());
            executionButtonPanel.add(jButton);
            executionButtonPanel.add(Box.createHorizontalStrut(10));
            executionButtonPanel.add(getExecutedRulesLabel());
        }
        return executionButtonPanel;
    }

    public class ExecutionActionListener implements ActionListener{

        public void actionPerformed(ActionEvent ev) {
            ExecuteRSW sw = new ExecuteRSW(_formGenerator);
            sw.execute();
            _formGenerator.getViewer().setBusy(OpenEHRLanguageManager.getMessage("Executing")+"...", sw);
        }
    }

    private JLinkLabel getExecutedRulesLabel(){
        if (executedRulesLabel==null){
            executedRulesLabel = new JLinkLabel();
            executedRulesLabel.setIcon(OpenEHRImageUtil.RULE_ICON);
            executedRulesLabel.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    List<RuleReference> ruleReferences = _formGenerator.getLastRulesFired();
                    if (ruleReferences!=null && !ruleReferences.isEmpty()){
                        Window owner = null;
                        if (_formGenerator.getViewer() instanceof Window){
                            owner = (Window)_formGenerator.getViewer();
                        }
                        Map<String,Map<String, String>> rulesMap = new HashMap<String, Map<String,String>>();
                        for (RuleReference ruleReference : ruleReferences) {
                            for (String lang : _formGenerator.getSupportedLanguages()) {
                                Map<String, String> auxMap = rulesMap.get(lang);
                                if (auxMap==null){
                                    auxMap = new LinkedHashMap<String, String>();
                                    rulesMap.put(lang, auxMap);
                                }
                                ReadableRule readableRule = getReadableRule(ruleReference, lang);
                                String ruleName = readableRule.getTermDefinition().getTerms().get(ruleReference.getGTCode()).getText();
                                String ruleStr = readableRule.toString();
                                auxMap.put(ruleName, ruleStr);
                            }
                        }
                        DialogRuleExecutionList dialog =
                                new DialogRuleExecutionList(owner, rulesMap, false);
                        dialog.setVisible(true);
                    }
                }
            });
            executedRulesLabel.setVisible(false);
        }
        return executedRulesLabel;
    }

    public ReadableRule getReadableRule(RuleReference ruleReference, String lang){
        ReadableGuide readableGuide =
                _formGenerator.getReadableGuideMap().get(ruleReference.getGuideId()).get(lang);
        if (readableGuide==null){//Language does not exist, fall back to English
            readableGuide =
                    _formGenerator.getReadableGuideMap().get(ruleReference.getGuideId()).get("en");
        }
        return readableGuide.getReadableRules().get(ruleReference.getGTCode());
    }

    public void updateResults(RuleExecutionResult result){
        if (result!=null && result.getFiredRules()!=null){
            setResultElements(result.getArchetypeReferences());
            int numRulesFired = result.getFiredRules().size();
            getExecutedRulesLabel().setText("("+numRulesFired+")");
            StringBuffer sb = new StringBuffer();
            sb.append("<HTML><b>"+OpenEHRLanguageManager.getMessage("ExecutionLog")+":</b><br>");
            String lang = _formGenerator.getLanguage();
            for (RuleReference ruleReference : result.getFiredRules()) {
                ReadableGuide readableGuide =
                        _formGenerator.getReadableGuideMap().get(ruleReference.getGuideId()).get(lang);
                if (readableGuide==null){//Language does not exist, fall back to English
                    readableGuide =
                            _formGenerator.getReadableGuideMap().get(ruleReference.getGuideId()).get("en");
                }
                String ruleName = readableGuide.getTermDefinition().getTerms().get(ruleReference.getGTCode()).getText();
                sb.append("&nbsp;&nbsp;"+ruleName+"<br>");
            }
            sb.append("</HTML>");
            getExecutedRulesLabel().setToolTipText(sb.toString());
            executedRulesLabel.setVisible(true);
        }else{
            setResultElements(null);
            getExecutedRulesLabel().setVisible(false);
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