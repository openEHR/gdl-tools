package se.cambio.cds.gdl.model.readable.rule.lines;

import se.cambio.cds.gdl.model.readable.rule.lines.elements.GTCodeRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.GTCodeDefiner;


public class FiredRuleInstantiationRuleLine extends RuleLine implements GTCodeDefiner {

    private GTCodeRuleLineElement _gtCodeRuleLineElement = null;

    public FiredRuleInstantiationRuleLine() {
        super("", "");
        _gtCodeRuleLineElement = new GTCodeRuleLineElement(this);
    }

    public GTCodeRuleLineElement getGTCodeRuleLineElement() {
        return _gtCodeRuleLineElement;
    }

    public String getGTCode() {
        return getGTCodeRuleLineElement().getValue();
    }

    public void setGTCode(String term) {
        getGTCodeRuleLineElement().setValue(term);
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