package se.cambio.cds.gdl.model.expression;

import java.util.List;

public class AssignmentExpressionList extends ExpressionItem {

	/**
     *
     */
    private static final long serialVersionUID = 1L;
	public AssignmentExpressionList(List<AssignmentExpression> assignmentExpressions) {
		super();
        this.assignmentExpressions = assignmentExpressions;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((assignmentExpressions == null) ? 0 : assignmentExpressions.hashCode());
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
		AssignmentExpressionList other = (AssignmentExpressionList) obj;
		if (assignmentExpressions == null) {
			if (other.assignmentExpressions != null)
				return false;
		} else if (!assignmentExpressions.equals(other.assignmentExpressions))
			return false;
		return true;
	}
	
	public String toString() {
		StringBuffer buf = new StringBuffer();
        buf.append("(");
        if (assignmentExpressions!=null){
            String prefix = "";
            for(AssignmentExpression assignmentExpression: assignmentExpressions){
                buf.append(prefix);
                buf.append(assignmentExpression);
                prefix = ";";
            }
        }
        buf.append(")");
		return buf.toString();
	}

    public List<AssignmentExpression> getAssignmentExpressions() {
        return assignmentExpressions;
    }

    public void setAssignmentExpressions(List<AssignmentExpression> assignmentExpressions) {
        this.assignmentExpressions = assignmentExpressions;
    }

    private List<AssignmentExpression> assignmentExpressions;
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