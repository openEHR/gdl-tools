package se.cambio.openehr.util.exceptions;

import java.io.PrintStream;
import java.io.PrintWriter;


public class InternalErrorException extends RuntimeException {

    private static final long serialVersionUID = -916751219990677911L;
    private Exception encapsulatedException;

    public InternalErrorException(Exception exception) {
        encapsulatedException = exception;
    }

    public String getMessage() {
        return "Internal error : " + encapsulatedException.getMessage();
    }

    public Exception getEncapsulatedException() {
        return encapsulatedException;
    }

    public void printStackTrace() {
        printStackTrace(System.err);
    }

    public void printStackTrace(PrintStream printStream) {
        super.printStackTrace(printStream);
        printStream.println("***Information about encapsulated exception***");
        encapsulatedException.printStackTrace(printStream);
    }

    public void printStackTrace(PrintWriter printWriter) {
        super.printStackTrace(printWriter);
        printWriter.println("***Information about encapsulated exception***");
        encapsulatedException.printStackTrace(printWriter);
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