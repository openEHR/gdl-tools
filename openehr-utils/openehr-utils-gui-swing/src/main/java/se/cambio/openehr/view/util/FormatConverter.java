package se.cambio.openehr.view.util;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.DvCount;
import org.openehr.rm.datatypes.quantity.DvOrdinal;
import org.openehr.rm.datatypes.quantity.DvProportion;
import org.openehr.rm.datatypes.quantity.DvQuantity;
import org.openehr.rm.datatypes.quantity.datetime.DvDate;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.openehr.rm.datatypes.quantity.datetime.DvTime;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.openehr.rm.datatypes.text.DvText;
import se.cambio.openehr.util.OpenEHRNumberFormat;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.Normalizer;
import java.text.SimpleDateFormat;
import java.util.*;


public class FormatConverter {

    private static long secondInMillis = 1000;
    private static long minuteInMillis = secondInMillis * 60;
    private static long hourInMillis = minuteInMillis * 60;
    private static long dayInMillis = hourInMillis * 24;
    private static long monthInMillis = dayInMillis * 30;
    private static long yearInMillis = dayInMillis * 365;

    public static Integer toInt(String str) throws Exception {
        try {
            return Integer.parseInt(str.trim());
        } catch (NumberFormatException ex) {
            throw new RuntimeException(ex);
        }
    }

    public static Double toDouble(String str)
            throws NumberFormatException {
        if (str.contains(",")) {
            str = str.replace(",", ".");
        }
        return Double.parseDouble(str.trim());
    }

    public static String toComaDouble(String str)
            throws NumberFormatException {
        if (str.contains(".")) {
            str = str.replace(".", ",");
        }
        return str;
    }

    public static Short toShort(String str) {
        try {
            return Short.parseShort(str.trim());
        } catch (NumberFormatException ex) {
            return (short) 0;
        }
    }

    public static Boolean toBoolean(String str) {
        try {
            return str.trim().equals("1");
        } catch (NumberFormatException ex) {
            return null;
        }
    }

    public static Short getAge(Calendar birthDate) {
        Calendar fechaAct = Calendar.getInstance();
        boolean restaUno = false;
        if (fechaAct.get(Calendar.DAY_OF_MONTH) - birthDate.get(Calendar.DAY_OF_MONTH) < 0) {
            restaUno = true;
        }
        restaUno = fechaAct.get(Calendar.MONTH) - (birthDate.get(Calendar.MONTH) + (restaUno ? 1 : 0)) < 0;
        return (short) ((fechaAct.get(Calendar.YEAR) - (birthDate.get(Calendar.YEAR) + (restaUno ? 1 : 0))));
    }

    public static String toString(Properties properties) {
        StringBuilder resultStr = new StringBuilder();
        ArrayList<String> lista = new ArrayList<>();
        for (Object id : properties.keySet()) {
            lista.add((String) id);
        }
        Collections.sort(lista);
        for (String id : lista) {
            Object value = properties.get(id);
            resultStr.append(resultStr).append(id).append("=").append(value).append("\n");
        }
        return resultStr.toString();
    }

    public static Calendar toCalendar(Date date) {
        if (date == null) {
            return null;
        }
        Calendar newDate = Calendar.getInstance();
        newDate.setTime(date);
        return newDate;
    }


    public static Calendar toCalendar(String date) {
        String delimChar = "/";
        if (date.indexOf("-") > 0) {
            delimChar = "-";
        }
        return toCalendar(date, delimChar);
    }


    public static Calendar toCalendar(String date, String delimChar) throws InternalErrorException {
        Calendar newDate = Calendar.getInstance();
        try {
            StringTokenizer st = new StringTokenizer(date, delimChar);

            String str = null;

            str = st.nextToken();
            newDate.set(Calendar.DAY_OF_MONTH, Integer.parseInt(str));

            str = st.nextToken();
            newDate.set(Calendar.MONTH, Integer.parseInt(str) - 1);

            str = st.nextToken();
            newDate.set(Calendar.YEAR, Integer.parseInt(str));

            return newDate;
        } catch (Exception ex) {
            try {
                DateFormat dateFormat = DateFormat.getDateInstance(DateFormat.MEDIUM);
                newDate.setTime(dateFormat.parse(date));
                return newDate;
            } catch (Exception e2) {
                throw new InternalErrorException(new Exception("Incorrect date: " + date));
            }
        }
    }

    public static String toCSVString(Calendar fecha) {
        return
                "" + fecha.get(Calendar.DAY_OF_MONTH)
                        + "/" + (fecha.get(Calendar.MONTH) + 1)
                        + "/" + fecha.get(Calendar.YEAR);
    }

    public static Date toDate(Calendar date) {
        if (date == null) {
            return null;
        }
        return date.getTime();
    }

    public static String toSorteableString(Calendar fecha) {
        return "" + fecha.get(Calendar.YEAR)
                + "-" + fecha.get(Calendar.MONTH)
                + "-" + (fecha.get(Calendar.DAY_OF_MONTH) < 10 ? "0" : "") + fecha.get(Calendar.DAY_OF_MONTH);
    }

    public static String toStdMinString(Long time) {
        DateFormat dateFormat = new SimpleDateFormat("mm:ss:SSSS");
        return dateFormat.format(time);
    }

    public static String toStdHourString(Long time) {
        DateFormat dateFormat = new SimpleDateFormat("HH:mm:ss");
        return dateFormat.format(time);
    }

    public static String toStdDiaString(Long time) {
        DateFormat dateFormat = new SimpleDateFormat("DD:HH:mm:ss");
        return dateFormat.format(time);
    }

    public static String toStdString(Calendar fecha) {
        return toStdString(fecha.getTime());
    }

    public static String toStdString(Date fecha) {
        DateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
        return dateFormat.format(fecha);
    }

    public static String toStdStringWithHour(Calendar fecha) {
        return toStdStringWithHour(fecha.getTime());
    }

    public static String toStdStringWithHour(Date fecha) {
        return DateFormat.getInstance().format(fecha);
    }

    public static long anhosTranscurridosEntre(Calendar time1, Calendar time2) {
        if (time1 != null && time2 != null) {
            long l1 = time1.getTime().getTime();
            long l2 = time2.getTime().getTime();
            long diff = l2 - l1;
            return diff / yearInMillis;
        } else {
            return 0;
        }
    }


    public static long mesesTranscurridosEntre(Calendar time1, Calendar time2) {
        if (time1 != null && time2 != null) {
            long l1 = time1.getTime().getTime();
            long l2 = time2.getTime().getTime();
            long diff = l2 - l1;
            diff = diff % yearInMillis;
            return diff / monthInMillis;
        } else {
            return 0;
        }
    }


    public static long getNumDaysBetweenDates(Calendar time1, Calendar time2) {
        if (time1 != null && time2 != null) {
            long l1 = time1.getTime().getTime();
            long l2 = time2.getTime().getTime();
            long diff = l2 - l1;
            diff = diff % yearInMillis;
            diff = diff % monthInMillis;
            return diff / dayInMillis;
        } else {
            return 0;
        }
    }

    public static boolean isSameDay(Calendar fecha1, Calendar fecha2) {
        return (fecha1.get(Calendar.YEAR) == fecha2.get(Calendar.YEAR))
                && (fecha1.get(Calendar.MONTH) == fecha2.get(Calendar.MONTH))
                && (fecha1.get(Calendar.DAY_OF_MONTH) == fecha2.get(Calendar.DAY_OF_MONTH));
    }

    public static boolean isBirthday(Calendar fecha) {
        Calendar today = Calendar.getInstance();
        return (fecha.get(Calendar.MONTH) == today.get(Calendar.MONTH))
                && (fecha.get(Calendar.DAY_OF_MONTH) == today.get(Calendar.DAY_OF_MONTH));
    }


    public static String textWithoutPunctuation(String str) {
        return Normalizer.normalize(str.toLowerCase(), Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", "");
    }

    public static String getReadableValue(DataValue dv) {
        if (dv instanceof DvCodedText) {
            CodePhrase cp = ((DvCodedText) dv).getDefiningCode();
            return ((DvCodedText) dv).getValue();
        } else if (dv instanceof DvOrdinal) {
            return ((DvOrdinal) dv).getSymbol().getValue();
        } else if (dv instanceof DvQuantity) {
            DvQuantity dvQuantity = ((DvQuantity) dv);
            DecimalFormat format = getDecimalFormat(dvQuantity.getPrecision());
            return format.format(dvQuantity.getMagnitude()) + " " + dvQuantity.getUnits();
        } else if (dv instanceof DvProportion) {
            DvProportion dvProportion = ((DvProportion) dv);
            DecimalFormat format = getDecimalFormat(dvProportion.getPrecision());
            return format.format(dvProportion.getNumerator()) + "/" + format.format(dvProportion.getDenominator());
        } else if (dv instanceof DvDateTime) {
            DateFormat df = DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT, Locale.getDefault());
            Date date = ((DvDateTime) dv).getDateTime().toDate();
            return df.format(date);
        } else if (dv instanceof DvDate) {
            DateFormat df = DateFormat.getDateInstance(DateFormat.SHORT, Locale.getDefault());
            Date date = ((DvDate) dv).getDateTime().toDate();
            return df.format(date);
        } else if (dv instanceof DvTime) {
            DateFormat df = DateFormat.getTimeInstance(DateFormat.SHORT, Locale.getDefault());
            Date date = ((DvTime) dv).getDateTime().toDate();
            return df.format(date);
        } else if (dv instanceof DvCount) {
            return "" + ((DvCount) dv).getMagnitude();
        } else if (dv instanceof DvText) {
            return dv.toString();
        } else if (dv != null) {
            return dv.toString();
        } else {
            return null;
        }
    }

    private static DecimalFormat getDecimalFormat(int precision) {
        return OpenEHRNumberFormat.getDecimalFormat(precision);
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