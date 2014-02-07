package se.cambio.cds.gdl.model.readable.rule.lines;

import se.cambio.cds.gdl.model.expression.BinaryExpression;
import se.cambio.cds.gdl.model.expression.ExpressionItem;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ConditionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.OperatorRuleLine;
import se.cambio.openehr.util.OpenEHRLanguageManager;


public class OrOperatorRuleLine extends ExpressionRuleLine implements ConditionRuleLine, OperatorRuleLine{

    private BranchRuleLine leftRuleLineBranch = null;
    private BranchRuleLine rightRuleLineBranch = null;
    
    public OrOperatorRuleLine() {
	super(OpenEHRLanguageManager.getMessage("OrOperator"), 
		OpenEHRLanguageManager.getMessage("OrOperatorDesc"));
	leftRuleLineBranch = new BranchRuleLine();
	rightRuleLineBranch = new BranchRuleLine();
	this.addChildRuleLine(leftRuleLineBranch);
	this.addChildRuleLine(rightRuleLineBranch);
    }

    public BranchRuleLine getLeftRuleLineBranch() {
        return leftRuleLineBranch;
    }

    public BranchRuleLine getRightRuleLineBranch() {
        return rightRuleLineBranch;
    }
    public void setCommented(boolean commented){
	super.setCommented(commented);
	getLeftRuleLineBranch().setCommented(commented);
	getRightRuleLineBranch().setCommented(commented);
    }
    
    public ExpressionItem toExpressionItem(){
	return new BinaryExpression(
		    convertToExpressionItem(getLeftRuleLineBranch().getChildrenRuleLines(),0), 
		    convertToExpressionItem(getRightRuleLineBranch().getChildrenRuleLines(),0),
		    OperatorKind.OR);
    }
    
    public String toString(){
	StringBuffer sb = new StringBuffer();
	sb.append("((");
	for (RuleLine ruleLine : getLeftRuleLineBranch().getChildrenRuleLines()) {
	    sb.append(ruleLine.toString());
	}
	sb.append(") "+OpenEHRLanguageManager.getMessage("OrRLE")+" (");
	for (RuleLine ruleLine : getRightRuleLineBranch().getChildrenRuleLines()) {
	    sb.append(ruleLine.toString());
	}
	sb.append("))");
	return sb.toString();
    }
    
    public String toHTMLString(int level){
	StringBuffer sb = new StringBuffer();
	sb.append(getLevelSpace(level)+"((<br>");
	for (RuleLine ruleLine : getLeftRuleLineBranch().getChildrenRuleLines()) {
	    sb.append(ruleLine.toHTMLString(level+1)+"<br>");
	}
	sb.append(getLevelSpace(level)+")"+getLevelSpace(level)+"<b>"+OpenEHRLanguageManager.getMessage("OrRLE")+"</b> (<br>");
	for (RuleLine ruleLine : getRightRuleLineBranch().getChildrenRuleLines()) {
	    sb.append(ruleLine.toHTMLString(level+1)+"<br>");
	}
	sb.append(getLevelSpace(level)+"))");
	return sb.toString();
    }
}/*
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