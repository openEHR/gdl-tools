package se.cambio.cds.gdl.model;

import lombok.Data;
import se.cambio.cds.gdl.model.expression.AssignmentExpression;
import se.cambio.cds.gdl.model.expression.ExpressionItem;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
public class Rule implements Serializable {

    private static final long serialVersionUID = 1L;
    private String id;
    private List<String> when;
    private List<String> then;
    private int priority;

    private List<ExpressionItem> whenStatements;
    private List<AssignmentExpression> thenStatements;

    public List<String> getWhen() {
        if (whenStatements == null || whenStatements.size() == 0) {
            return when;
        }
        List<String> lines = new ArrayList<>();
        for (ExpressionItem item : whenStatements) {
            lines.add(item.toString());
        }
        return lines;
    }

    public List<String> getThen() {
        if (thenStatements == null || thenStatements.size() == 0) {
            return then;
        }
        List<String> lines = new ArrayList<>();
        for (ExpressionItem item : thenStatements) {
            lines.add(item.toString());
        }
        return lines;
    }

    public void setWhenStatements(List<ExpressionItem> whenStatements) {
        this.whenStatements = whenStatements;
        List<String> lines = new ArrayList<>();
        if (this.whenStatements != null) {
            for (ExpressionItem item : whenStatements) {
                lines.add(item.toString());
            }
        }
        this.when = lines;
    }

    public void setThenStatements(List<AssignmentExpression> thenStatements) {
        this.thenStatements = thenStatements;
        List<String> lines = new ArrayList<>();
        if (this.thenStatements != null) {
            for (ExpressionItem item : thenStatements) {
                lines.add(item.toString());
            }
        }
        this.then = lines;
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