package se.cambio.cds.util.misc;

/**
 * @author iago.corbal
 *
 */

public final class CDSSEnviroment {

	private static boolean isDebuggin = false;
	private static boolean isTesting = false;
	private static boolean _traceSW = false;
	private static boolean _productionEnviroment = false;
	
	public static boolean isDebuggin(){
		return isDebuggin;
	}

	public static boolean isTesting(){
		return isTesting;
	}

	public static boolean isTracingSW(){
		return _traceSW;
	}

	public static boolean isProductionEnviroment(){
		return _productionEnviroment;
	}
	
	public static void setDebug(boolean debug){
		isDebuggin = debug;
		if (debug) {
			System.out.println("*** Debug ON ***");
		}
	}

	public static void setTraceSW(boolean traceSW){
		_traceSW = traceSW;
	}

	public static void setTesting(boolean test){
		isTesting = test;
		if (test) {
			System.out.println("*** Test ON ***");
		}else{
			System.out.println("*** Test OFF ***");
		}
	}
	
	public static void setProductionEnviroment(boolean productionEnviroment){
		_productionEnviroment = productionEnviroment;
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