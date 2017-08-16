package se.cambio.cds.gdl.model.readable.rule.lines;

import lombok.extern.slf4j.Slf4j;
import se.cambio.cds.gdl.model.expression.AssignmentExpression;
import se.cambio.cds.gdl.model.expression.CreateInstanceExpression;
import se.cambio.cds.gdl.model.expression.Variable;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.CDSEntryRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.GTCodeRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.StaticTextRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ActionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.InstantiationRuleLine;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.util.misc.CDSLanguageManager;

import java.util.ArrayList;
import java.util.List;


@Slf4j
public class CreateInstanceActionRuleLine extends AssignmentExpressionRuleLine implements ActionRuleLine, InstantiationRuleLine {

    private CDSEntryRuleLineElement cdsEntryRuleLineElement = null;

    public CreateInstanceActionRuleLine() {
        super(CDSLanguageManager.getMessage("CreateInstance"),
                CDSLanguageManager.getMessage("CreateInstanceDesc"));
        cdsEntryRuleLineElement = new CDSEntryRuleLineElement(this);
        getRuleLineElements().add(new StaticTextRuleLineElement(this, "CreateInstanceRLE"));
        getRuleLineElements().add(cdsEntryRuleLineElement);
    }

    public void setCDSEntryGTCodeRuleLineElementValue(GTCodeRuleLineElement value) {
        if (cdsEntryRuleLineElement != null) {
            cdsEntryRuleLineElement.setValue(value);
        }
    }

    public ArchetypeReference getArchetypeReference() {
        return cdsEntryRuleLineElement.getArchetypeReference();
    }

    @Override
    public AssignmentExpression toAssignmentExpression() throws IllegalStateException {
        ArchetypeReference archetypeReference = getArchetypeReference();
        if (archetypeReference != null) {
            String name = archetypeReference.getIdArchetype();
            Variable var = new Variable(
                    cdsEntryRuleLineElement.getValue().getValue(),
                    null, name, CreateInstanceExpression.FUNCTION_CREATE_NAME);
            List<AssignmentExpression> assignmentExpressions = new ArrayList<>();
            if (!getChildrenRuleLines().getRuleLines().isEmpty()) {
                for (RuleLine childRuleLine : getChildrenRuleLines().getRuleLines()) {
                    AssignmentExpressionRuleLine assignmentExpressionRuleLine = (AssignmentExpressionRuleLine) childRuleLine;
                    assignmentExpressions.add(assignmentExpressionRuleLine.toAssignmentExpression());
                }
            } else {
                log.debug("No assignment rules on create instance action rule");
                return null;
            }
            return new CreateInstanceExpression(
                    var,
                    assignmentExpressions);
        } else {
            log.debug("No archetype reference set on create instance action rule");
            return null;
        }
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        for (RuleLine ruleLine : getChildrenRuleLines().getRuleLines()) {
            sb.append(ruleLine.toString());
            sb.append("\n");
        }
        return sb.toString();
    }

    public String toHTMLString(int level, String lang) {
        StringBuilder sb = new StringBuilder();
        sb.append(toHTMLStringSingle(level, lang)).append("<br/>");
        String prefix = "";
        for (RuleLine ruleLine : getChildrenRuleLines().getRuleLines()) {
            sb.append(prefix);
            sb.append(ruleLine.toHTMLString(level + 1, lang));
            prefix = "<br/>";
        }
        return sb.toString();
    }

    private String toHTMLStringSingle(int level, String lang) {
        return super.toHTMLString(level, lang);
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