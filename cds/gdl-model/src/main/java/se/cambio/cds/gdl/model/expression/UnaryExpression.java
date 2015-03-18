package se.cambio.cds.gdl.model.expression;

/**
 * A unary expression only has a single operand and an operator
 * 
 * @author rong.chen
 * 
 */
public class UnaryExpression extends ExpressionItem {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;
	public static UnaryExpression create(ExpressionItem operand, 
			OperatorKind operator) {
		return new UnaryExpression(operand, operator);
	}
	
	public UnaryExpression(ExpressionItem operand, OperatorKind operator) {
		super();
		this.operand = operand;
		this.operator = operator;
	}

	/**
	 * @return the operand
	 */
	public ExpressionItem getOperand() {
		return operand;
	}

	/**
	 * @return the operator
	 */
	public OperatorKind getOperator() {
		return operator;
	}
	
	public String toString() {
		StringBuffer buf = new StringBuffer();
		buf.append(operator.getSymbol());
		buf.append("(");
		buf.append(operand);
		buf.append(")");
		return buf.toString();
	}

	private ExpressionItem operand;
	private OperatorKind operator;
	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((operand == null) ? 0 : operand.hashCode());
		result = prime * result
				+ ((operator == null) ? 0 : operator.hashCode());
		return result;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		UnaryExpression other = (UnaryExpression) obj;
		if (operand == null) {
			if (other.operand != null)
				return false;
		} else if (!operand.equals(other.operand))
			return false;
		if (operator != other.operator)
			return false;
		return true;
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