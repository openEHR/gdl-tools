/**
 * @author iago.corbal
 *
 */
package se.cambio.openehr.util;

import org.apache.log4j.Logger;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Observable;


public class ExceptionHandler extends Observable{

    private static ExceptionHandler _instance = null;

    private ExceptionHandler(){

    }

    public static void handle(InternalErrorException e){
	Logger.getLogger(ExceptionHandler.class).warn(e.getMessage());
	//if (Logger.getLogger(ExceptionHandler.class).isDebugEnabled()){
	    e.printStackTrace();
	//}
    }

    public static void handle(Throwable th){
	Logger.getLogger(ExceptionHandler.class).warn(th.getMessage());
	//if (Logger.getLogger(ExceptionHandler.class).isDebugEnabled()){
	    th.printStackTrace();
	//}
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