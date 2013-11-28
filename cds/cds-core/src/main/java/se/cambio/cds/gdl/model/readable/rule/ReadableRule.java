package se.cambio.cds.gdl.model.readable.rule;

import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.OpenEHRLanguageManager;

import java.util.ArrayList;
import java.util.List;

public class ReadableRule {

    private TermDefinition _termDefinition = null;
    private String gtCode = null;
    private List<RuleLine> _conditionRuleLines = null;
    private List<RuleLine> _actionRuleLines = null;
    private boolean commented = false;

    public ReadableRule(TermDefinition termDefinition, String gtCode){
	this.gtCode = gtCode;
	_conditionRuleLines = new ArrayList<RuleLine>();
	_actionRuleLines = new ArrayList<RuleLine>();
	_termDefinition = termDefinition;
    }
    
    public String getGTCode() {
	return gtCode;
    }
    
    public List<RuleLine> getConditionRuleLines() {
	return _conditionRuleLines;
    }

    public List<RuleLine> getActionRuleLines() {
	return _actionRuleLines;
    }

    public boolean isCommented(){
	return commented;
    }

    public void setCommented(boolean commented){
	this.commented = commented;
    }

    public TermDefinition getTermDefinition() {
        return _termDefinition;
    }
    
    private String getName(String gtCode){
	Term term = getTermDefinition().getTerms().get(gtCode);
	if (term!=null){
	    return term.getText();
	}else{
	    ExceptionHandler.handle(new Exception("Unknow term for gtCode='"+gtCode+"'"));
	    return "*UNKNOWN*";
	}
    }
    
    public String toString(){
	StringBuffer sb = new StringBuffer();
	sb.append("<b><font color='#999999'>"+OpenEHRLanguageManager.getMessage("Rule")+"</font><font> "+getName(gtCode)+"</font></b><br>");
	sb.append("<b><font color='#999999'>"+OpenEHRLanguageManager.getMessage("When")+"</font></b><br>");
	for (RuleLine ruleLine : getConditionRuleLines()) {
	    sb.append(ruleLine.toHTMLString(1)+"</div><br>");
	}
	sb.append("<b><font color='#999999'>"+OpenEHRLanguageManager.getMessage("Then")+"</font></b><br>");
	for (RuleLine ruleLine : getActionRuleLines()) {
	    sb.append(ruleLine.toHTMLString(1)+"</div><br>");
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