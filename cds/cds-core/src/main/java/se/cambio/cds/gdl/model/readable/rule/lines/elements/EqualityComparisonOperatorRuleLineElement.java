package se.cambio.cds.gdl.model.readable.rule.lines.elements;

import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.openehr.util.OpenEHRLanguageManager;

import java.util.ArrayList;
import java.util.Collection;


public class EqualityComparisonOperatorRuleLineElement extends RuleLineElementWithValue<OperatorKind> implements SingleSelectionRuleElement<OperatorKind> {

    private Collection<OperatorKind> _operators = null;

    public EqualityComparisonOperatorRuleLineElement(RuleLine ruleLine) {
        super(ruleLine, null);
        _operators = new ArrayList<>();
        _operators.add(OperatorKind.EQUALITY);
        _operators.add(OperatorKind.INEQUAL);
    }

    @Override
    public String getResolvedName(OperatorKind item, String language) {
        if (item != null) {
            return item.getSymbol();
        } else {
            return null;
        }
    }

    @Override
    public String getResolvedDescription(OperatorKind item, String language) {
        if (item != null) {
            return item.getName();
        } else {
            return null;
        }
    }

    public Collection<OperatorKind> getItems() {
        return _operators;
    }

    @Override
    public String toString() {
        return getOperatorText(getLanguage());
    }

    private String getOperatorText(String language) {
        if (getValue() != null) {
            switch (getValue()) {
                case EQUALITY:
                    return OpenEHRLanguageManager.getMessageWithLanguage("EqualsRLE", language);
                case INEQUAL:
                    return OpenEHRLanguageManager.getMessageWithLanguage("NotEqualsRLE", language);
                default:
                    return "??";
            }
        } else {
            return "??";
        }
    }

    @Override
    public String getLabelTextHTML(String lang) {
        return getOperatorText(lang);
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