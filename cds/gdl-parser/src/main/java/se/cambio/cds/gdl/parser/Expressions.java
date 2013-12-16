package se.cambio.cds.gdl.parser;

import se.cambio.cds.gdl.model.expression.ExpressionItem;

import java.io.Reader;
import java.io.StringReader;
import java.util.List;

public class Expressions {

    public static List<ExpressionItem> parseBooleanExpressions(String value) throws Exception {
        List<ExpressionItem> items;
        ExpressionParser parser = new ExpressionParser(convert(value));
        items = parser.parseBooleanExpressions();
        return items;
    }

    public static List<ExpressionItem> parseArithmeticExpressions(String value) throws Exception {
        List<ExpressionItem> items;
        ExpressionParser parser = new ExpressionParser(convert(value));
        items = parser.parseArithmeticExpressions();
        return items;
    }

    public static ExpressionItem parse(String value) throws Exception {
        ExpressionParser parser = new ExpressionParser(convert(value));
        ExpressionItem item = parser.parse();
        return item;
    }

    private static Reader convert(String value) throws Exception {
        return new StringReader(value);
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