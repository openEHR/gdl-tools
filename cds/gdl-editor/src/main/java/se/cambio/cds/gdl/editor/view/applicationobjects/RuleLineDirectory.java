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

    private Collection<RuleLine> _selectableDefinitions = null;
    private Collection<RuleLine> _selectableConditions = null;
    private Collection<RuleLine> _selectableActions = null;
    private static RuleLineDirectory _instance =null;

    private RuleLineDirectory(){

    }

    public static Collection<RuleLine> getSelectableDefinitions(){
        if (getDelegate()._selectableDefinitions==null){
            getDelegate()._selectableDefinitions = new ArrayList<RuleLine>();
            getDelegate()._selectableDefinitions.add(new ArchetypeInstantiationRuleLine());
            getDelegate()._selectableDefinitions.add(new ArchetypeElementInstantiationRuleLine(null));
            getDelegate()._selectableDefinitions.add(new WithElementPredicateAttributeDefinitionRuleLine());
            getDelegate()._selectableDefinitions.add(new WithElementPredicateFunctionDefinitionRuleLine());
            getDelegate()._selectableDefinitions.add(new WithElementPredicateExistsDefinitionRuleLine());
            getDelegate()._selectableDefinitions.add(new WithElementPredicateExpressionDefinitionRuleLine(null));
        }
        return getDelegate()._selectableDefinitions;
    }
    public static Collection<RuleLine> getSelectableConditions(){
        if (getDelegate()._selectableConditions==null){
            getDelegate()._selectableConditions = new ArrayList<RuleLine>();
            getDelegate()._selectableConditions.add(new ElementComparisonWithDVConditionRuleLine());
            getDelegate()._selectableConditions.add(new ElementComparisonWithNullValueConditionRuleLine());
            getDelegate()._selectableConditions.add(new ElementComparisonWithElementConditionRuleLine());
            getDelegate()._selectableConditions.add(new ElementAttributeComparisonConditionRuleLine());
            getDelegate()._selectableConditions.add(new ElementInitializedConditionRuleLine());
            //getDelegate()._selectableConditions.add(new ForAllOperatorRuleLine()); //Deprecated
            getDelegate()._selectableConditions.add(new OrOperatorRuleLine());
        }
        return getDelegate()._selectableConditions;
    }


    public static Collection<RuleLine> getSelectableActions(){
        if (getDelegate()._selectableActions==null){
            getDelegate()._selectableActions = new ArrayList<RuleLine>();
            getDelegate()._selectableActions.add(new SetElementWithDataValueActionRuleLine());
            getDelegate()._selectableActions.add(new SetElementWithNullValueActionRuleLine());
            getDelegate()._selectableActions.add(new SetElementWithElementActionRuleLine());
            getDelegate()._selectableActions.add(new SetElementAttributeActionRuleLine());
        }
        return getDelegate()._selectableActions;
    }

    public static boolean isDirectoryRuleLine(RuleLine ruleLine){
        return getSelectableDefinitions().contains(ruleLine) ||
                getSelectableConditions().contains(ruleLine) ||
                getSelectableActions().contains(ruleLine);
    }

    public static ImageIcon getIconForRuleLine(RuleLine ruleLine){
        if (ruleLine instanceof DefinitionsRuleLine){
            return GDLEditorImageUtil.SOURCE_ICON;
        }else if (ruleLine instanceof ConditionRuleLine){
            return GDLEditorImageUtil.CONDITION_ICON;
        }else if (ruleLine instanceof ActionRuleLine){
            return GDLEditorImageUtil.ACTION_ICON;
        }else{
            return GDLEditorImageUtil.EMPTY_ICON;
        }
    }

    public static RuleLineDirectory getDelegate(){
        if (_instance==null){
            _instance = new RuleLineDirectory();
        }
        return _instance;
    }

    public static boolean checkRuleLineCompatibility(RuleLine ruleLine, RuleLine ruleLineParent){
        if (ruleLineParent==null){
            if(ruleLine instanceof ArchetypeElementInstantiationRuleLine ||
                    ruleLine instanceof WithElementPredicateAttributeDefinitionRuleLine ||
                    ruleLine instanceof WithElementPredicateExpressionDefinitionRuleLine ||
                    ruleLine instanceof WithElementPredicateExistsDefinitionRuleLine ||
                    ruleLine instanceof WithElementPredicateFunctionDefinitionRuleLine){
                return false;
            }else{
                return true;
            }
        }else if (ruleLine instanceof ArchetypeInstantiationRuleLine){
            return false;
        }else if (ruleLine instanceof ArchetypeElementInstantiationRuleLine){
            if (ruleLineParent instanceof ArchetypeInstantiationRuleLine){
                return true;
            }else{
                return false;
            }
        }else if (ruleLine instanceof WithElementPredicateAttributeDefinitionRuleLine){
            if (ruleLineParent instanceof ArchetypeInstantiationRuleLine){
                return true;
            }else{
                return false;
            }
        }else if (ruleLine instanceof WithElementPredicateExpressionDefinitionRuleLine){
            if (ruleLineParent instanceof ArchetypeInstantiationRuleLine){
                return true;
            }else{
                return false;
            }
        }else{
            return true;
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