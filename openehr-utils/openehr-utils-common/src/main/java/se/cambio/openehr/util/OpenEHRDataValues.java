package se.cambio.openehr.util;

public class OpenEHRDataValues {


    //DATA VALUES
    public static final String DV_TEXT = "DV_TEXT";
    public static final String DV_CODED_TEXT = "DV_CODED_TEXT";
    public static final String DV_ORDINAL = "DV_ORDINAL";
    public static final String DV_DURATION = "DV_DURATION";
    public static final String DV_COUNT = "DV_COUNT";
    public static final String DV_DATE_TIME = "DV_DATE_TIME";
    public static final String DV_DATE = "DV_DATE";
    public static final String DV_TIME = "DV_TIME";
    public static final String DV_BOOLEAN = "DV_BOOLEAN";
    public static final String DV_PROPORTION = "DV_PROPORTION";
    public static final String DV_QUANTITY = "DV_QUANTITY";
    public static final String DV_URI = "DV_URI";
    public static final String DV_STATE = "DV_STATE";
    public static final String DV_IDENTIFIER = "DV_IDENTIFIER";
    public static final String DV_PARSABLE = "DV_PARSABLE";

    public enum DataValue {
        DV_TEXT, DV_CODED_TEXT, DV_ORDINAL,
        DV_DURATION, DV_COUNT, DV_DATE_TIME,
        DV_DATE, DV_TIME, DV_BOOLEAN, DV_PROPORTION,
        DV_QUANTITY, DV_URI, DV_STATE, DV_IDENTIFIER,
        DV_PARSABLE
    }

    //Functions
    public static final String FUNCTION_COUNT = "count";

    //Attributes
    public static final String VALUE_ATT = "value";
    public static final String SYMBOL_VALUE_ATT = "symbolValue";
    public static final String UNITS_ATT = "units";
    public static final String PRECISION_ATT = "precision";
    public static final String MAGNITUDE_ATT = "magnitude";
    public static final String NUMERATOR_ATT = "numerator";
    public static final String DENOMINATOR_ATT = "denominator";
    public static final String TYPE_ATT = "type";
    public static final String TEMINOLOGYID_ATT = "terminologyId";
    public static final String CODE_ATT = "code";
    public static final String DEFINING_CODE_ATT = "defining_code";
    public static final String MAPPINGS_ATT = "mappings";
    public static final String FORMALISM_ATT = "formalism";

    public static final String YEAR_ATT = "year";
    public static final String MONTH_ATT = "month";
    public static final String DAY_ATT = "day";
    public static final String HOUR_ATT = "hour";
    public static final String MINUTE_ATT = "minute";
    public static final String SECOND_ATT = "second";
    public static final String FRACTIONAL_SECOND_ATT = "fractionalSecond";
    public static final String TIMEZONE_ATT = "timeZone";

    public static boolean isDataValue(String dataValueId) {
        for (DataValue c : DataValue.values()) {
            if (c.name().equals(dataValueId)) {
                return true;
            }
        }
        return false;
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