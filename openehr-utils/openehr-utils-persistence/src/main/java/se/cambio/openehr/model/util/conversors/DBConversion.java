package se.cambio.openehr.model.util.conversors;

import java.math.BigDecimal;
import java.sql.Date;
import java.sql.Timestamp;
import java.util.Calendar;
import java.util.Collection;


/**
 * @author iago.corbal
 * 
 * Contains utility methods to convert objects from/to DB 
 */

public class DBConversion {

	final static Short SHORT_FALSE = new Short((short)0);
	final static Short SHORT_TRUE = new Short((short)1);

	public static Short toShort(Boolean valor){
		if (valor==null) return null;
		return (valor.booleanValue())?SHORT_TRUE:SHORT_FALSE;
	}

	public static Boolean toBoolean(Short valor){
		if (valor==null) return null;
		return (valor.equals(SHORT_FALSE))?Boolean.FALSE:Boolean.TRUE;
	}

	public static Calendar toCalendar(Timestamp fecha){
		if (fecha==null) return null;
		Calendar fechaC = Calendar.getInstance();
		fechaC.setTimeInMillis(fecha.getTime());
		return fechaC;
	}

	public static Timestamp toTimestamp(Calendar fecha){
		if (fecha==null) return null;
		return new Timestamp(fecha.getTime().getTime());
	}	

	public static Date toDate(Calendar fecha){
		if (fecha==null) return null;
		return new Date(fecha.getTime().getTime());
	}

    public static java.util.Date toDate(java.sql.Date date){
        if (date==null) return null;
        return new java.util.Date(date.getTime());
    }

    public static java.sql.Date toSQLDate(java.util.Date date){
        if (date==null) return null;
        return new java.sql.Date(date.getTime());
    }

    public static java.util.Date toDate(Timestamp timestamp){
        if (timestamp==null) return null;
        return new java.util.Date(timestamp.getTime());
    }

     public static Timestamp toTimestamp(java.util.Date date){
        if (date==null) return null;
        return new Timestamp(date.getTime());
    }

    public static Calendar toCalendar(Date fecha){
		if (fecha==null) return null;
		Calendar cal = Calendar.getInstance();
		cal.setTime(fecha);
		return cal;
	}	
	
	public static String toString(Character value){
		if (value==null) return null;
		return value.toString();
	}

	public static Character toChar(String value){
		if (value==null) return null;
		return new Character(value.charAt(0));
	}	

	public static Double toDouble(BigDecimal value){
		if (value==null) return null;
		return new Double(value.doubleValue());
	}
	
	public static Long toLong(BigDecimal value){
		if (value==null) return null;
		return new Long(value.longValue());
	}
	
	public static BigDecimal toBigDecimal(Long value){
		if (value==null) return null;
		return new BigDecimal(value);
	}
	
	public static <E> String toSQLArrayString(Collection<E> objects){
		if (objects==null){
			return "()";
		}
		StringBuilder sb = new StringBuilder();
		sb.append('(');
		for (E object : objects) {
			sb.append(object);
			sb.append(",");
		}
		if (sb.length()>1){
			return sb.substring(0, sb.length()-1)+")";
		}else{
			return sb.append(')').toString();
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