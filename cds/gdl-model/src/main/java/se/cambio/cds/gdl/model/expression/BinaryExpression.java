package se.cambio.cds.gdl.model.expression;


/**
 * This class represents a binary expression that has two operands, 
 * left and right, and an operator.
 * 
 * @author rong.chen
 *
 */
public class BinaryExpression extends ExpressionItem {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;
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
	
	/**
	 * @return the left
	 */
	public ExpressionItem getLeft() {
		return left;
	}
	/**
	 * @param left the left to set
	 */
	public void setLeft(ExpressionItem left) {
		this.left = left;
	}
	/**
	 * @return the right
	 */
	public ExpressionItem getRight() {
		return right;
	}
	/**
	 * @param right the right to set
	 */
	public void setRight(ExpressionItem right) {
		this.right = right;
	}
	/**
	 * @return the operator
	 */
	public OperatorKind getOperator() {
		return operator;
	}
	/**
	 * @param operator the operator to set
	 */
	public void setOperator(OperatorKind operator) {
		this.operator = operator;
	}
	
	public String toString() {
		StringBuffer buf = new StringBuffer();
		if(left instanceof BinaryExpression) {
			buf.append("(");
			buf.append(left.toString());
			buf.append(")");
		} else {
		    if (left!=null){
			buf.append(left.toString());
		    }else{
			throw new IllegalStateException("Left item == null");
		    }
		}
		
		if(OperatorKind.IS_A == operator) {
			buf.append(" ");
		}
		if (operator!=null){
		    buf.append(operator.getSymbol());
		}else{
		    throw new IllegalStateException("Operator == null");
		}
		if(OperatorKind.IS_A == operator) {
			buf.append(" ");
		}
		
		if(right instanceof BinaryExpression) {
			buf.append("(");
			buf.append(right.toString());
			buf.append(")");
		} else {
		    if (right!=null){
			buf.append(right.toString());
		    }else{
			throw new IllegalStateException("Right item == null");
		    }
		}
		return buf.toString();
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((left == null) ? 0 : left.hashCode());
		result = prime * result
				+ ((operator == null) ? 0 : operator.hashCode());
		result = prime * result + ((right == null) ? 0 : right.hashCode());
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
		BinaryExpression other = (BinaryExpression) obj;
		if (left == null) {
			if (other.left != null)
				return false;
		} else if (!left.equals(other.left))
			return false;
		if (operator != other.operator)
			return false;
		if (right == null) {
			if (other.right != null)
				return false;
		} else if (!right.equals(other.right))
			return false;
		return true;
	}
	
	private ExpressionItem left;
	private ExpressionItem right;
	private OperatorKind operator;	
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