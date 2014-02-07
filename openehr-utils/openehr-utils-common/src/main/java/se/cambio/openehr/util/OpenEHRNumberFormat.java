package se.cambio.openehr.util;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;

import org.openehr.rm.datatypes.quantity.DvQuantity;

public class OpenEHRNumberFormat  {

    
    public static DecimalFormat getDecimalFormat(){
	return getDecimalFormat(null);
    }
    public static DecimalFormat getDecimalFormat(Integer precision){
	DecimalFormatSymbols custom = new DecimalFormatSymbols();
	custom.setDecimalSeparator(DvQuantity.DECIMAL_SEPARATOR);
	DecimalFormat format = new DecimalFormat();
	format.setDecimalFormatSymbols(custom);
	format.setGroupingUsed(false);
	if (precision!=null){
	    format.setMinimumFractionDigits(precision);
	    format.setMaximumFractionDigits(precision);
	}
	return format;
    }

    
    public static String roundToStr(double unrounded, int precision){
	StringBuffer roundSB= new StringBuffer();
	roundSB.append(unrounded);
	String origStr = roundSB.toString();
	if (origStr.contains(".")){
	    int numDecimals = origStr.length()-origStr.indexOf(".")-1;
	    if (numDecimals!=precision){
		if (numDecimals>precision){
		    if (precision>0){
			roundSB = new StringBuffer(origStr.substring(0, origStr.indexOf(".")+precision+1));
		    }else{
			roundSB = new StringBuffer(origStr.substring(0, origStr.indexOf(".")));
		    }
		}else {
		    appendZeros(roundSB, precision-numDecimals);
		}
	    }
	}else{
	    if (precision>0){
		roundSB.append(".");
		appendZeros(roundSB, precision);
	    }
	}
	return roundSB.toString();
    }

    private static void appendZeros(StringBuffer roundSB, int precision){
	for (int i =0; i<precision;i++) {
	    roundSB.append("0");
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