package se.cambio.cds.gdl.model.expression;

import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = false)
@Data
public class UnaryExpression extends ExpressionItem {

    private static final long serialVersionUID = 1L;
    private ExpressionItem operand;
    private OperatorKind operator;

    public static UnaryExpression create(ExpressionItem operand,
                                         OperatorKind operator) {
        return new UnaryExpression(operand, operator);
    }

    public UnaryExpression(ExpressionItem operand, OperatorKind operator) {
        super();
        this.operand = operand;
        this.operator = operator;
    }

    @Override
    public String toString() {
        return operator.getSymbol() + "(" + operand + ")";
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