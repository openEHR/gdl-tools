package se.cambio.openehr.util;

import org.openehr.rm.datatypes.quantity.DvQuantity;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;

public class OpenEHRNumberFormat {


    public static final char DV_QUANTITY_DECIMAL_SEPARATOR = '.';

    public static DecimalFormat getDecimalFormat() {
        return getDecimalFormat(null);
    }

    public static DecimalFormat getDecimalFormat(Integer precision) {
        DecimalFormatSymbols custom = new DecimalFormatSymbols();
        custom.setDecimalSeparator(DV_QUANTITY_DECIMAL_SEPARATOR);
        DecimalFormat format = new DecimalFormat();
        format.setDecimalFormatSymbols(custom);
        format.setGroupingUsed(false);
        if (precision != null) {
            format.setMinimumFractionDigits(precision);
            format.setMaximumFractionDigits(precision);
        }
        return format;
    }


    public static String toStringUsingPrecision(double value, int precision) {
        String strValue = new BigDecimal(value).toString();
        if (strValue.contains(".")) {
            int numDecimals = strValue.length() - strValue.indexOf(".") - 1;
            if (numDecimals != precision) {
                if (numDecimals > precision) {
                    DecimalFormat format = new DecimalFormat();
                    format.setMinimumFractionDigits(precision);
                    format.setMaximumFractionDigits(precision);
                    DecimalFormatSymbols dfs = format.getDecimalFormatSymbols();
                    dfs.setDecimalSeparator(DvQuantity.DECIMAL_SEPARATOR);
                    format.setDecimalFormatSymbols(dfs);
                    format.setGroupingUsed(false);
                    return format.format(value);
                } else {
                    StringBuilder roundSB = new StringBuilder();
                    roundSB.append(strValue.substring(0, strValue.indexOf(".")))
                            .append(".");
                    appendZeros(roundSB, precision - numDecimals);
                    return roundSB.toString();
                }
            } else {
                return strValue;
            }
        } else {
            if (precision > 0) {
                StringBuilder valueWithZeroesSB = new StringBuilder();
                valueWithZeroesSB.append(strValue);
                valueWithZeroesSB.append(".");
                appendZeros(valueWithZeroesSB, precision);
                return valueWithZeroesSB.toString();
            } else {
                return strValue;
            }
        }
    }

    private static void appendZeros(StringBuilder roundSB, int precision) {
        for (int i = 0; i < precision; i++) {
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