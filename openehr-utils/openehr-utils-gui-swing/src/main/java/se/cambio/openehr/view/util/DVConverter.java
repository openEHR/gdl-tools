package se.cambio.openehr.view.util;

import java.util.Calendar;

import org.openehr.rm.datatypes.quantity.datetime.DvDate;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.openehr.rm.datatypes.quantity.datetime.DvTime;

public class DVConverter {

    public static DvDateTime getDvDateTime(Calendar cal){
	return new DvDateTime(
		cal.get(Calendar.YEAR),
		cal.get(Calendar.MONTH)+1,
		cal.get(Calendar.DAY_OF_MONTH),
		cal.get(Calendar.HOUR_OF_DAY),
		cal.get(Calendar.MINUTE),
		cal.get(Calendar.SECOND),
		cal.getTimeZone());
    }

    public static DvDate getDvDate(Calendar cal){
	return new DvDate(
		cal.get(Calendar.YEAR),
		cal.get(Calendar.MONTH)+1,
		cal.get(Calendar.DAY_OF_MONTH));
    }

    public static DvTime getDvTime(Calendar cal){
	return new DvTime(
		cal.get(Calendar.HOUR_OF_DAY),
		cal.get(Calendar.MINUTE),
		cal.get(Calendar.SECOND),
		cal.getTimeZone());
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