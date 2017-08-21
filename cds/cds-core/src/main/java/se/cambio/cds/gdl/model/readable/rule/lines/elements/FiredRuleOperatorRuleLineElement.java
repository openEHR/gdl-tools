package se.cambio.cds.gdl.model.readable.rule.lines.elements;

import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.util.misc.CDSLanguageManager;

import java.util.ArrayList;
import java.util.Collection;

public class FiredRuleOperatorRuleLineElement extends RuleLineElementWithValue<OperatorKind> implements SingleSelectionRuleElement<OperatorKind> {

    public FiredRuleOperatorRuleLineElement(RuleLine ruleLine) {
        super(ruleLine, null);
    }

    @Override
    public String getResolvedName(OperatorKind operatorKind, String lang) {
        if (OperatorKind.FIRED.equals(operatorKind)) {
            return CDSLanguageManager.getMessageWithLanguage("HasBeenFiredRLE", lang);
        } else if (OperatorKind.NOT_FIRED.equals(operatorKind)) {
            return CDSLanguageManager.getMessageWithLanguage("HasNotBeenFiredRLE", lang);
        } else {
            throw new RuntimeException("Unknown operator '" + operatorKind + "'");
        }
    }

    @Override
    public String getResolvedDescription(OperatorKind item, String lang) {
        return getResolvedName(item, lang);
    }

    @Override
    public Collection<OperatorKind> getItems() {
        Collection<OperatorKind> items = new ArrayList<OperatorKind>();
        items.add(OperatorKind.FIRED);
        items.add(OperatorKind.NOT_FIRED);
        return items;
    }

    @Override
    public String getLabelText(String language) {
        if (getValue() != null) {
            return getResolvedName(getValue(), language);
        } else {
            return super.getLabelText(language);
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