package se.cambio.cds.gdl.model.expression;

import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@EqualsAndHashCode(callSuper = false)
@Data
public class FunctionalExpression extends ExpressionItem {
    private static final long serialVersionUID = 1L;
    private Function function;
    private List<ExpressionItem> items = new ArrayList<>();

    public static FunctionalExpression create(Function function) {
        return new FunctionalExpression(function);
    }

    public static FunctionalExpression create(Function function, ExpressionItem item) {
        List<ExpressionItem> items = new ArrayList<>();
        items.add(item);
        return new FunctionalExpression(function, items);
    }

    public static FunctionalExpression create(Function function, List<ExpressionItem> items) {
        return new FunctionalExpression(function, items);
    }

    public FunctionalExpression(Function function) {
        this(function, null);
    }

    public FunctionalExpression(Function function, List<ExpressionItem> items) {
        if (function == null) {
            throw new IllegalArgumentException("null function");
        }
        this.function = function;
        if (items != null) {
            this.items = new ArrayList<>(items);
        }
    }

    public String toString() {
        StringBuilder buf = new StringBuilder();
        buf.append(function);
        buf.append("(");
        ExpressionItem item;
        if (items != null) {
            for (int i = 0, j = items.size(); i < j; i++) {
                item = items.get(i);
                if (item instanceof BinaryExpression) {
                    buf.append("(");
                    buf.append(item.toString());
                    buf.append(")");
                } else {
                    buf.append(item.toString());
                }
                if (i != j - 1) {
                    buf.append(",");
                }
            }
        }
        buf.append(")");
        return buf.toString();
    }

    public List<ExpressionItem> getItems() {
        return Collections.unmodifiableList(items);
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