package se.cambio.cds.gdl.model.readable.rule;

import lombok.extern.slf4j.Slf4j;
import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.gdl.model.readable.ReadableGuide;
import se.cambio.cds.gdl.model.readable.rule.lines.FiredRuleInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.openehr.util.OpenEHRLanguageManager;

@Slf4j
public class ReadableRule {

    private final ReadableGuide readableGuide;
    private TermDefinition _termDefinition = null;
    private String gtCode = null;
    private RuleLineCollection _conditionRuleLines = null;
    private RuleLineCollection _actionRuleLines = null;
    private FiredRuleInstantiationRuleLine definitionRuleLine;

    private boolean commented = false;

    public ReadableRule(TermDefinition termDefinition, String gtCode, ReadableGuide readableGuide) {
        this.gtCode = gtCode;
        _termDefinition = termDefinition;
        this.readableGuide = readableGuide;
    }

    public ReadableGuide getReadableGuide() {
        return readableGuide;
    }

    public String getGTCode() {
        return gtCode;
    }

    public RuleLineCollection getConditionRuleLines() {
        if (_conditionRuleLines == null) {
            _conditionRuleLines = new RuleLineCollection(getReadableGuide());
        }
        return _conditionRuleLines;
    }

    public RuleLineCollection getActionRuleLines() {
        if (_actionRuleLines == null) {
            _actionRuleLines = new RuleLineCollection(getReadableGuide());
        }
        return _actionRuleLines;
    }

    public FiredRuleInstantiationRuleLine getDefinitionRuleLine() {
        if (definitionRuleLine == null) {
            definitionRuleLine = new FiredRuleInstantiationRuleLine();
            definitionRuleLine.setReadableGuide(getReadableGuide());
            definitionRuleLine.setGTCode(getGTCode());
        }
        return definitionRuleLine;
    }

    public boolean isCommented() {
        return commented;
    }

    public void setCommented(boolean commented) {
        this.commented = commented;
    }

    public TermDefinition getTermDefinition() {
        return _termDefinition;
    }

    private String getName(String gtCode) {
        Term term = getTermDefinition().getTerms().get(gtCode);
        if (term != null) {
            return term.getText();
        } else {
            log.error("Unknown term for gtCode='" + gtCode + "'");
            return "*UNKNOWN*";
        }
    }

    public String toString() {
        return toHTMLString(getReadableGuide().getArchetypeManager().getUserConfigurationManager().getLanguage());
    }

    public String toHTMLString(String lang) {
        StringBuffer sb = new StringBuffer();
        sb.append("<b><font color='#999999'>" + OpenEHRLanguageManager.getMessageWithLanguage("Rule", lang) + "</font><font> " + getName(gtCode) + "</font></b><br>");
        sb.append("<b><font color='#999999'>" + OpenEHRLanguageManager.getMessageWithLanguage("When", lang) + "</font></b><br>");
        for (RuleLine ruleLine : getConditionRuleLines().getRuleLines()) {
            sb.append(ruleLine.toHTMLString(1, lang) + "<br>");
        }
        sb.append("<b><font color='#999999'>" + OpenEHRLanguageManager.getMessageWithLanguage("Then", lang) + "</font></b><br>");
        for (RuleLine ruleLine : getActionRuleLines().getRuleLines()) {
            sb.append(ruleLine.toHTMLString(1, lang) + "<br>");
        }
        return sb.toString();
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