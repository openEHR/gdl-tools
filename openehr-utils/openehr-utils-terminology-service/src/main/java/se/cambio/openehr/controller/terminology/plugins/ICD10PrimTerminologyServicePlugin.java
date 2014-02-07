package se.cambio.openehr.controller.terminology.plugins;

import org.openehr.rm.datatypes.text.CodePhrase;
import se.cambio.openehr.util.exceptions.InvalidCodeException;
import se.cambio.openehr.util.exceptions.UnsupportedLanguageException;
import se.cambio.openehr.util.exceptions.UnsupportedTerminologyException;


public class ICD10PrimTerminologyServicePlugin extends CSVTerminologyServicePlugin{

    private static String TERMINOLOGY_ID = "ICD10prim";

    public ICD10PrimTerminologyServicePlugin() {
        super(TERMINOLOGY_ID);
    }

    protected boolean checkSubclassOf(String as, String bs)
            throws UnsupportedTerminologyException, InvalidCodeException {
        String cleanAS = cleanUpCode(as);
        if (invalidCode(cleanAS)){
            throw new InvalidCodeException("Invalid ICD10prim code: " + as);
        }
        String cleanBS = cleanUpCode(bs);
        if (invalidCode(cleanBS)){
            throw new InvalidCodeException("Invalid ICD10prim code: " + bs);
        }
        return cleanAS.contains(cleanBS); //No need to check parent nodes
        //return super.checkSubclassOf(cleanAS, cleanBS);
    }


    public String retrieveTerm(String code, CodePhrase language)
            throws UnsupportedTerminologyException,
            UnsupportedLanguageException {
        code = cleanUpCode(code);
        return super.getDescription(code);
    }

    private String cleanUpCode(String code) {
        code = code.replace("-", "");
        code = code.replace(".", "");
        while (code.length()>1 && invalidCode(code)){
            code = code.substring(0, code.length()-1);
        }
        return code;
    }

    public boolean isValidCodePhrase(CodePhrase codePhrase) {
        String code = cleanUpCode(codePhrase.getCodeString());
        return  TERMINOLOGY_ID.equals(codePhrase.getTerminologyId().getValue()) && !invalidCode(code);
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