package se.cambio.cds.gdl.model.expression;


import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = false)
@Data
public class BinaryExpression extends ExpressionItem {

    private static final long serialVersionUID = 1L;
    private ExpressionItem left;
    private ExpressionItem right;
    private OperatorKind operator;

    public static BinaryExpression create(ExpressionItem left, ExpressionItem right,
                                          OperatorKind operator) {
        return new BinaryExpression(left, right, operator);
    }

    public BinaryExpression(ExpressionItem left, ExpressionItem right,
                            OperatorKind operator) {
        this.left = left;
        this.right = right;
        this.operator = operator;
    }

    public String toString() {
        StringBuilder buf = new StringBuilder();
        if (left instanceof BinaryExpression
                || left instanceof UnaryExpression
                || left instanceof FunctionalExpression) {
            buf.append("(");
            buf.append(left.toString());
            buf.append(")");
        } else {
            if (left != null) {
                buf.append(left.toString());
            } else {
                throw new IllegalStateException("Left item == null");
            }
        }

        if (OperatorKind.IS_A == operator) {
            buf.append(" ");
        }
        if (operator != null) {
            buf.append(operator.getSymbol());
        } else {
            throw new IllegalStateException("Operator == null");
        }
        if (OperatorKind.IS_A == operator) {
            buf.append(" ");
        }

        if (right instanceof BinaryExpression
                || right instanceof UnaryExpression
                || right instanceof FunctionalExpression) {
            buf.append("(");
            buf.append(right.toString());
            buf.append(")");
        } else {
            if (right != null) {
                buf.append(right.toString());
            } else {
                throw new IllegalStateException("Right item == null");
            }
        }
        return buf.toString();
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