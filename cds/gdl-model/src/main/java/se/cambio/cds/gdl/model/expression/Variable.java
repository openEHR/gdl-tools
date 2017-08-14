package se.cambio.cds.gdl.model.expression;

import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = false)
@Data
public class Variable extends ExpressionItem {

    private static final long serialVersionUID = 1L;
    private String name;
    private String code;
    private String path;
    private String attribute;

    public static Variable createByCode(String code) {
        return new Variable(code);
    }

    public Variable(String code, String name, String path, String attribute) {
        this.name = name;
        this.code = code;
        this.path = path;
        this.attribute = attribute;
    }

    public Variable(String code, String name) {
        this(code, name, null);
    }

    public Variable(String code, String name, String path) {
        this(code, name, path, null);
    }

    public Variable(String code) {
        this.code = code;
    }

    public String toString() {
        StringBuilder buf = new StringBuilder();
        if (code != null) {
            buf.append("$");
            buf.append(code);
            if (name != null) {
                buf.append("|");
                buf.append(name);
                buf.append("|");
            }
            if (attribute != null) {
                buf.append(".");
                buf.append(attribute);
            }

        } else {
            buf.append(path);
        }
        return buf.toString();
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