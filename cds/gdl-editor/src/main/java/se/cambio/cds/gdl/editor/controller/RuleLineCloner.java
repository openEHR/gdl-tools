package se.cambio.cds.gdl.editor.controller;

import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.GTCodeDefiner;

import com.rits.cloning.Cloner;

public class RuleLineCloner {

    private static RuleLineCloner _instance = null;
    private Cloner _cloner = null;

    private RuleLineCloner(){
    }

    private static Cloner getCloner(){
        if (getDelegate()._cloner==null){
            getDelegate()._cloner = new Cloner();
        }
        return getDelegate()._cloner;
    }

    public static RuleLine clone(RuleLine ruleLine){
        RuleLine clonedRuleLine = RuleLineCloner.getCloner().deepClone(ruleLine);
        if (ruleLine instanceof GTCodeDefiner){
            GTCodeDefiner tdRuleLine = (GTCodeDefiner)clonedRuleLine;
            String gtCode = EditorManager.getActiveGDLEditor().createNextGTCode();
            tdRuleLine.setGTCode(gtCode);
        }
        clonedRuleLine.setTermDefinition(EditorManager.getActiveGDLEditor().getCurrentTermDefinition());
        return clonedRuleLine;
    }

    public static RuleLineCloner getDelegate(){
        if (_instance==null){
            _instance = new RuleLineCloner();
        }
        return _instance;
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