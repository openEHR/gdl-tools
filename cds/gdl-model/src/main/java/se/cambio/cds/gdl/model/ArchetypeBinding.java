package se.cambio.cds.gdl.model;

import lombok.Data;
import se.cambio.cds.gdl.model.expression.ExpressionItem;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Data
public class ArchetypeBinding implements Serializable {

    private static final long serialVersionUID = 1L;

    private String id;
    private String archetypeId;
    private String templateId;
    private String domain;
    private Map<String, ElementBinding> elements;
    private List<String> predicates;
    private List<ExpressionItem> predicateStatements;

    public List<String> getPredicates() {
        if (predicateStatements == null || predicateStatements.size() == 0) {
            return predicates;
        }
        List<String> lines = new ArrayList<>();
        for (ExpressionItem item : predicateStatements) {
            lines.add(item.toString());
        }
        return lines;
    }

    public void setPredicateStatements(List<ExpressionItem> predicateStatements) {
        this.predicateStatements = predicateStatements;
        List<String> lines = new ArrayList<>();
        if (predicateStatements != null) {
            for (ExpressionItem item : predicateStatements) {
                lines.add(item.toString());
            }
        }
        this.predicates = lines;
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