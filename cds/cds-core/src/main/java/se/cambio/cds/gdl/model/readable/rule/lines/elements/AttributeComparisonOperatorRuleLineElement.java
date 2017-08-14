package se.cambio.cds.gdl.model.readable.rule.lines.elements;

import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;


public class AttributeComparisonOperatorRuleLineElement extends AbstractComparisonOperatorRuleLineElement {

    public AttributeComparisonOperatorRuleLineElement(RuleLine ruleLine) {
        super(ruleLine);
        addOperator(OperatorKind.EQUALITY);
        addOperator(OperatorKind.INEQUAL);
        addOperator(OperatorKind.LESS_THAN);
        addOperator(OperatorKind.LESS_THAN_OR_EQUAL);
        addOperator(OperatorKind.GREATER_THAN);
        addOperator(OperatorKind.GREATER_THAN_OR_EQUAL);
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