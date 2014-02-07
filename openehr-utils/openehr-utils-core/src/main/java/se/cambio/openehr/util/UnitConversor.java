package se.cambio.openehr.util;

import java.text.ParsePosition;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import se.cambio.openehr.util.exceptions.InternalErrorException;



public class UnitConversor {

    @SuppressWarnings("unchecked")
    public static Double convertUnits(Double valueA, String unitsA, String unitsB) throws InternalErrorException{
	try{
	    UnitFormat unitFormat = UnitFormat.getStandard();
	    @SuppressWarnings("rawtypes")
	    Unit obj = unitFormat.parse(unitsA, new ParsePosition(0));
	    @SuppressWarnings("rawtypes")
	    Unit obj2 = unitFormat.parse(unitsB, new ParsePosition(0));
	    return obj.getConverterTo(obj2).convert(valueA);
	}catch(Exception e){
	    throw new InternalErrorException(e);
	}
    }
/*
    public static void main(String args[]){
	try {
	    Double result = UnitConversor.convertUnits(new Double(2.0), "lb", "g");
	    System.out.println(result);
	} catch (InternalErrorException e) {
	    e.printStackTrace();
	}
    }
*/
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