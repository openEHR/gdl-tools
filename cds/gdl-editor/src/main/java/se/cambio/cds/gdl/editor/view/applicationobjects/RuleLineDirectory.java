package se.cambio.cds.gdl.editor.view.applicationobjects;

import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.model.readable.rule.lines.*;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ActionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ConditionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.DefinitionsRuleLine;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Collection;

public class RuleLineDirectory {

    private Collection<RuleLine> selectableDefinitions = null;
    private ArrayList<RuleLine> selectablePreconditions;
    private Collection<RuleLine> selectableConditions = null;
    private Collection<RuleLine> selectableActions = null;
    private ArrayList<RuleLine> selectableDefaultActions;
    private static RuleLineDirectory instance = null;

    private RuleLineDirectory() {

    }

    public static Collection<RuleLine> getSelectableDefinitions() {
        if (getDelegate().selectableDefinitions == null) {
            getDelegate().selectableDefinitions = new ArrayList<>();
            getDelegate().selectableDefinitions.add(new ArchetypeInstantiationRuleLine());
            getDelegate().selectableDefinitions.add(new ArchetypeElementInstantiationRuleLine(null));
            getDelegate().selectableDefinitions.add(new WithElementPredicateAttributeDefinitionRuleLine());
            getDelegate().selectableDefinitions.add(new WithElementPredicateFunctionDefinitionRuleLine());
            getDelegate().selectableDefinitions.add(new WithElementPredicateExistsDefinitionRuleLine());
            getDelegate().selectableDefinitions.add(new WithElementPredicateExpressionDefinitionRuleLine(null));
        }
        return getDelegate().selectableDefinitions;
    }

    public static Collection<RuleLine> getSelectablePreconditions() {
        if (getDelegate().selectablePreconditions == null) {
            getDelegate().selectablePreconditions = new ArrayList<>();
            getDelegate().selectablePreconditions.add(new ElementComparisonWithDVConditionRuleLine());
            getDelegate().selectablePreconditions.add(new ElementComparisonWithNullValueConditionRuleLine());
            getDelegate().selectablePreconditions.add(new ElementComparisonWithElementConditionRuleLine());
            getDelegate().selectablePreconditions.add(new ElementAttributeComparisonConditionRuleLine());
            getDelegate().selectablePreconditions.add(new ElementInitializedConditionRuleLine());
            getDelegate().selectablePreconditions.add(new OrOperatorRuleLine());
            getDelegate().selectablePreconditions.add(new NotOperatorRuleLine());
        }
        return getDelegate().selectablePreconditions;
    }

    public static Collection<RuleLine> getSelectableConditions() {
        if (getDelegate().selectableConditions == null) {
            getDelegate().selectableConditions = new ArrayList<>();
            getDelegate().selectableConditions.addAll(getSelectablePreconditions());
            getDelegate().selectableConditions.add(new FiredRuleConditionRuleLine());
        }
        return getDelegate().selectableConditions;
    }

    public static Collection<RuleLine> getSelectableDefaultActions() {
        if (getDelegate().selectableDefaultActions == null) {
            getDelegate().selectableDefaultActions = new ArrayList<>();
            getDelegate().selectableDefaultActions.add(new CreateInstanceActionRuleLine());
            getDelegate().selectableDefaultActions.add(new SetElementWithDataValueActionRuleLine());
            getDelegate().selectableDefaultActions.add(new SetElementWithNullValueActionRuleLine());
            getDelegate().selectableDefaultActions.add(new SetElementAttributeActionRuleLine());

        }
        return getDelegate().selectableDefaultActions;
    }

    public static Collection<RuleLine> getSelectableActions() {
        if (getDelegate().selectableActions == null) {
            getDelegate().selectableActions = new ArrayList<>();
            getDelegate().selectableActions.addAll(getSelectableDefaultActions());
            getDelegate().selectableActions.add(new SetElementWithElementActionRuleLine());
        }
        return getDelegate().selectableActions;
    }

    public static boolean isDirectoryRuleLine(RuleLine ruleLine) {
        return getSelectableDefinitions().contains(ruleLine)
                || getSelectableConditions().contains(ruleLine)
                || getSelectableActions().contains(ruleLine);
    }

    public static ImageIcon getIconForRuleLine(RuleLine ruleLine) {
        if (ruleLine instanceof DefinitionsRuleLine) {
            return GDLEditorImageUtil.SOURCE_ICON;
        } else if (ruleLine instanceof ConditionRuleLine) {
            return GDLEditorImageUtil.CONDITION_ICON;
        } else if (ruleLine instanceof ActionRuleLine) {
            return GDLEditorImageUtil.ACTION_ICON;
        } else {
            return GDLEditorImageUtil.EMPTY_ICON;
        }
    }

    private static RuleLineDirectory getDelegate() {
        if (instance == null) {
            instance = new RuleLineDirectory();
        }
        return instance;
    }

    public static boolean checkRuleLineCompatibility(RuleLine ruleLine, RuleLine ruleLineParent) {
        if (ruleLineParent == null) {
            return !(ruleLine instanceof ArchetypeElementInstantiationRuleLine
                    || ruleLine instanceof WithElementPredicateAttributeDefinitionRuleLine
                    || ruleLine instanceof WithElementPredicateExpressionDefinitionRuleLine
                    || ruleLine instanceof WithElementPredicateExistsDefinitionRuleLine
                    || ruleLine instanceof WithElementPredicateFunctionDefinitionRuleLine);
        } else if (ruleLine instanceof ArchetypeInstantiationRuleLine) {
            return false;
        } else if (ruleLine instanceof ArchetypeElementInstantiationRuleLine) {
            return ruleLineParent instanceof ArchetypeInstantiationRuleLine;
        } else if (ruleLine instanceof WithElementPredicateAttributeDefinitionRuleLine) {
            return ruleLineParent instanceof ArchetypeInstantiationRuleLine;
        } else if (ruleLine instanceof WithElementPredicateExpressionDefinitionRuleLine) {
            return ruleLineParent instanceof ArchetypeInstantiationRuleLine;
        } else {
            return !(ruleLineParent instanceof CreateInstanceActionRuleLine) || !(ruleLine instanceof CreateInstanceActionRuleLine);
        }
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