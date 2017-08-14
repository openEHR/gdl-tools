package se.cambio.cds.gdl.model;

import lombok.Data;
import org.apache.commons.lang.StringUtils;
import se.cambio.cds.gdl.model.expression.AssignmentExpression;
import se.cambio.cds.gdl.model.expression.ExpressionItem;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Data
public class GuideDefinition implements Serializable {

    private Map<String, ArchetypeBinding> archetypeBindings = new HashMap<>();
    private List<String> preConditions = new ArrayList<>();
    private List<String> defaultActions = new ArrayList<>();
    private Map<String, Rule> rules = new HashMap<>();
    private List<ExpressionItem> preConditionExpressions = new ArrayList<>();
    private List<AssignmentExpression> defaultActionExpressions = new ArrayList<>();

    public static final String ARCHETYPE_BINDING_PREFIX = "ab";

    private static final long serialVersionUID = 1L;

    public List<String> getPreConditions() {
        if (getPreConditionExpressions().isEmpty()) {
            return preConditions;
        }
        List<String> lines = new ArrayList<>();
        for (ExpressionItem item : preConditionExpressions) {
            lines.add(item.toString());
        }
        return lines;
    }

    public void setArchetypeBindings(Object archetypeBindings) {
        if (archetypeBindings instanceof Map) {
            changeArchetypeBindings((Map) archetypeBindings);
        } else if (archetypeBindings instanceof List) {
            changeArchetypeBindings((List) archetypeBindings);
        } else {
            throw new InternalError("Archetype bindings could not be set (unknown type '" + archetypeBindings.getClass().getName() + "').");
        }
    }

    public void changeArchetypeBindings(Map<String, ArchetypeBinding> archetypeBindings) {
        this.archetypeBindings = archetypeBindings;
    }

    public void changeArchetypeBindings(List<ArchetypeBinding> archetypeBindings) {
        Map<String, ArchetypeBinding> archetypeBindingsAux = new HashMap<>();
        int index = 1;
        for (ArchetypeBinding archetypeBinding : archetypeBindings) {
            String abCode = ARCHETYPE_BINDING_PREFIX + StringUtils.leftPad("" + (index++), 4, "0");
            archetypeBinding.setId(abCode);
            archetypeBindingsAux.put(abCode, archetypeBinding);
        }
        this.archetypeBindings = archetypeBindingsAux;
    }

    public void setPreConditionExpressions(List<ExpressionItem> preConditionExpressions) {
        this.preConditionExpressions = preConditionExpressions;
        List<String> lines = new ArrayList<>();
        if (preConditionExpressions != null) {
            for (ExpressionItem item : preConditionExpressions) {
                String str = item.toString();
                if (str != null) {
                    lines.add(str);
                }
            }
        }
        this.preConditions = lines;
    }

    public List<String> getDefaultActions() {
        if (getDefaultActionExpressions().isEmpty()) {
            return defaultActions;
        }
        List<String> lines = new ArrayList<>();
        for (ExpressionItem item : defaultActionExpressions) {
            lines.add(item.toString());
        }
        return lines;
    }

    public void setDefaultActionExpressions(List<AssignmentExpression> defaultActionExpressions) {
        this.defaultActionExpressions = defaultActionExpressions;
        List<String> lines = new ArrayList<>();
        if (defaultActionExpressions != null) {
            for (ExpressionItem item : defaultActionExpressions) {
                String str = item.toString();
                if (str != null) {
                    lines.add(str);
                }
            }
        }
        this.defaultActions = lines;
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