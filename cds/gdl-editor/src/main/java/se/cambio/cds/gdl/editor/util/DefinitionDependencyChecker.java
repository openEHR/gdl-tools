package se.cambio.cds.gdl.editor.util;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.model.readable.rule.ReadableRule;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeElementInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.*;
import se.cambio.cds.model.instance.ArchetypeReference;

import java.util.Iterator;

public class DefinitionDependencyChecker {

    public static boolean isBeingUsed(ArchetypeInstantiationRuleLine airl, GDLEditor controller) {
        boolean found = false;
        ArchetypeReference ar = airl.getArchetypeReference();
        Iterator<RuleLine> i = controller.getReadableGuide().getPreconditionRuleLines().getRuleLines().iterator();
        while (i.hasNext() && !found) {
            if (isBeingReferenced(ar, i.next())) {
                found = true;
            }
        }
        Iterator<ReadableRule> i2 = controller.getReadableGuide().getReadableRules().values().iterator();
        while (i2.hasNext() && !found) {
            ReadableRule rr = i2.next();
            Iterator<RuleLine> i3 = rr.getConditionRuleLines().getRuleLines().iterator();
            while (i3.hasNext() && !found) {
                if (isBeingReferenced(ar, i3.next())) {
                    found = true;
                }
            }
            i3 = rr.getActionRuleLines().getRuleLines().iterator();
            while (i3.hasNext() && !found) {
                if (isBeingReferenced(ar, i3.next())) {
                    found = true;
                }
            }
        }
        return found;
    }

    private static boolean isBeingReferenced(ArchetypeReference ar, RuleLine ruleLine) {
        boolean found = false;
        Iterator<RuleLineElement> i = ruleLine.getRuleLineElements().iterator();
        while (i.hasNext() && !found) {
            if (isBeingReferenced(ar, i.next())) {
                found = true;
            }
        }
        Iterator<RuleLine> i2 = ruleLine.getChildrenRuleLines().getRuleLines().iterator();
        while (i2.hasNext() && !found) {
            if (isBeingReferenced(ar, i2.next())) {
                found = true;
            }
        }
        return found;
    }

    private static boolean isBeingReferenced(ArchetypeReference ar, RuleLineElement ruleLineElement) {
        ArchetypeReference arAux = null;
        if (ruleLineElement instanceof ArchetypeReferenceRuleLineElement) {
            arAux = ((ArchetypeReferenceRuleLineElement) ruleLineElement).getArchetypeReference();
        } else if (ruleLineElement instanceof ArchetypeElementRuleLineElement) {
            arAux = ((ArchetypeElementRuleLineElement) ruleLineElement).getArchetypeReference();
        } else if (ruleLineElement instanceof ArchetypeElementAttributeRuleLineElement) {
            arAux = ((ArchetypeElementAttributeRuleLineElement) ruleLineElement).getArchetypeReference();
        }
        return arAux != null && arAux.equals(ar);
    }

    public static boolean isBeingUsed(ArchetypeElementInstantiationRuleLine aeirl, GDLEditor controller) {
        boolean found = false;
        String gtCode = aeirl.getGTCode();
        Iterator<RuleLine> i = controller.getReadableGuide().getPreconditionRuleLines().getRuleLines().iterator();
        while (i.hasNext() && !found) {
            if (isBeingReferenced(gtCode, i.next())) {
                found = true;
            }
        }
        Iterator<ReadableRule> i2 = controller.getReadableGuide().getReadableRules().values().iterator();
        while (i2.hasNext() && !found) {
            ReadableRule rr = i2.next();
            Iterator<RuleLine> i3 = rr.getConditionRuleLines().getRuleLines().iterator();
            while (i3.hasNext() && !found) {
                if (isBeingReferenced(gtCode, i3.next())) {
                    found = true;
                }
            }
            i3 = rr.getActionRuleLines().getRuleLines().iterator();
            while (i3.hasNext() && !found) {
                if (isBeingReferenced(gtCode, i3.next())) {
                    found = true;
                }
            }
        }
        return found;
    }

    private static boolean isBeingReferenced(String gtCode, RuleLine ruleLine) {
        boolean found = false;
        Iterator<RuleLineElement> i = ruleLine.getRuleLineElements().iterator();
        while (i.hasNext() && !found) {
            if (isBeingReferenced(gtCode, i.next())) {
                found = true;
            }
        }
        Iterator<RuleLine> i2 = ruleLine.getChildrenRuleLines().getRuleLines().iterator();
        while (i2.hasNext() && !found) {
            if (isBeingReferenced(gtCode, i2.next())) {
                found = true;
            }
        }
        return found;
    }

    private static boolean isBeingReferenced(String gtCode, RuleLineElement ruleLineElement) {
        String gtCodeAux = null;
        if (ruleLineElement instanceof ArchetypeElementRuleLineElement) {
            GTCodeRuleLineElement gtcrle = ((ArchetypeElementRuleLineElement) ruleLineElement).getValue();
            if (gtcrle != null) {
                gtCodeAux = gtcrle.getValue();
            }
        } else if (ruleLineElement instanceof ArchetypeElementAttributeRuleLineElement) {
            ArchetypeElementRuleLineElement aerle = ((ArchetypeElementAttributeRuleLineElement) ruleLineElement).getValue();
            if (aerle != null) {
                GTCodeRuleLineElement gtcrle = aerle.getValue();
                if (gtcrle != null) {
                    gtCodeAux = gtcrle.getValue();
                }
            }
        }
        return gtCodeAux != null && gtCodeAux.equals(gtCode);
    }

    public static boolean isBeingUsedInAction(ArchetypeInstantiationRuleLine airl, GDLEditor controller) {
        boolean found = false;
        ArchetypeReference ar = airl.getArchetypeReference();
        Iterator<ReadableRule> i2 = controller.getReadableGuide().getReadableRules().values().iterator();
        while (i2.hasNext() && !found) {
            ReadableRule rr = i2.next();
            Iterator<RuleLine> i3 = rr.getActionRuleLines().getRuleLines().iterator();
            while (i3.hasNext() && !found) {
                if (isBeingReferenced(ar, i3.next())) {
                    found = true;
                }
            }
        }
        return found;
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