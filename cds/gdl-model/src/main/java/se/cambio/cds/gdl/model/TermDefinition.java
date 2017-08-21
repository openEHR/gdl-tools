package se.cambio.cds.gdl.model;

import lombok.Data;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

@Data
public class TermDefinition implements Serializable {

    private static final long serialVersionUID = 1L;
    private String id;
    private Map<String, Term> terms;

    public TermDefinition() {

    }

    public Map<String, Term> getTerms() {
        if (terms == null) {
            terms = new HashMap<>();
        }
        return terms;
    }

    public String getTermText(String gtCode) {
        Term term = getTerms().get(gtCode);
        String text = term != null ? term.getText() : null;
        return unescape(text);
    }

    private String unescape(String desc) {
        if (desc != null) {
            return desc.replace("\\\"", "\"");
        } else {
            return "";
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