package se.cambio.openehr.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.Scanner;

public class PropertiesEx extends Properties {

    private static final long serialVersionUID = 1L;

    public void load(InputStream fis) throws IOException {
        Scanner in = new Scanner(fis, "UTF-8");
        ByteArrayOutputStream out = new ByteArrayOutputStream();

        while (in.hasNext()) {
            out.write(in.nextLine().replace("\\\\", "\\").replace("\\:", ":").replace("\\", "\\\\").getBytes("UTF-8"));
            out.write("\n".getBytes("UTF-8"));
        }

        InputStream is = new ByteArrayInputStream(out.toByteArray());
        super.load(is);
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