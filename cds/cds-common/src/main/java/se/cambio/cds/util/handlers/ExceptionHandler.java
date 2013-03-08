/**
 * @author iago.corbal
 *
 */
package se.cambio.cds.util.handlers;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Observable;

import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.misc.CDSSEnviroment;


public class ExceptionHandler extends Observable{

    private static ExceptionHandler _instance = null;

    private ExceptionHandler(){

    }

    public static void handle(InternalErrorException e){
	if (CDSSEnviroment.isDebuggin()){
	    e.printStackTrace();
	} else {
	    //TODO
	    //ErrLog.logException("InternalException",e);
	    e.printStackTrace();
	    notifyAdmin(e);
	}
    }

    public static void handle(Throwable th){
	if (CDSSEnviroment.isDebuggin()){
	    th.printStackTrace();
	} else {
	    //TODO
	    //ErrLog.logException("Unknown Exception(Throwable))",th);
	    th.printStackTrace();
	    notifyAdmin(th);
	}
    }

    public static void handleSilent(Throwable th){
	if (CDSSEnviroment.isDebuggin()){
	    th.printStackTrace();
	} else {
	    //TODO
	    //ErrLog.logException("Silent Exception",th);
	    th.printStackTrace();
	}
    }

    private static void notifyAdmin(Throwable th){
	StringWriter stringWriter = new StringWriter();
	PrintWriter printWriter = new PrintWriter(stringWriter);
	th.printStackTrace(printWriter);
	getDelegate().setChanged();
	getDelegate().notifyObservers(stringWriter.toString());
    }

    public static ExceptionHandler getDelegate(){
	if (_instance==null){
	    _instance = new ExceptionHandler();
	}
	return _instance;
    }
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  1.1 (the 'License'); you may not use this file except in compliance with
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