package se.cambio.openehr.util;

import org.openehr.jaxb.rm.DvParsable;
import org.openehr.rm.datatypes.basic.DvBoolean;
import org.openehr.rm.datatypes.quantity.DvCount;
import org.openehr.rm.datatypes.quantity.DvOrdinal;
import org.openehr.rm.datatypes.quantity.DvProportion;
import org.openehr.rm.datatypes.quantity.DvQuantity;
import org.openehr.rm.datatypes.quantity.datetime.DvDate;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.openehr.rm.datatypes.quantity.datetime.DvDuration;
import org.openehr.rm.datatypes.quantity.datetime.DvTime;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.openehr.rm.datatypes.text.DvText;
import org.openehr.rm.datatypes.uri.DvURI;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

public class OpenEHRDataValuesUI {

    private static OpenEHRDataValuesUI instance = null;
    private final HashMap<String, String> openEHRDVNames;
    private final HashMap<String, String> openEHRDVDescriptions;
    private final HashMap<String, ImageIcon> openEHRDVIcons;
    private final HashMap<String, String[]> openEHRDVFieldNames;
    private final HashMap<String, String> openEHRDVClassName;
    private final HashMap<String, String> openEHRDVIconName;

    private static final ArrayList<String> GENERIC_FUNCTIONS = new ArrayList<>();

    static {
        GENERIC_FUNCTIONS.add(OpenEHRDataValues.FUNCTION_COUNT);
    }

    private CodePhrase _languageCodePhrase = null;
    private CodePhrase _defaultLanguageCodePhrase = null;

    private OpenEHRDataValuesUI() {
        openEHRDVNames = new HashMap<>();
        openEHRDVDescriptions = new HashMap<>();
        openEHRDVIcons = new HashMap<>();
        openEHRDVFieldNames = new HashMap<>();
        openEHRDVClassName = new HashMap<>();
        openEHRDVIconName = new HashMap<>();

        openEHRDVNames.put(OpenEHRDataValues.DV_TEXT, OpenEHRLanguageManager.getMessage("DataValueText"));
        openEHRDVDescriptions.put(OpenEHRDataValues.DV_TEXT, OpenEHRLanguageManager.getMessage("DataValueTextDesc"));
        openEHRDVIcons.put(OpenEHRDataValues.DV_TEXT, OpenEHRImageUtil.DV_TEXT_ICON);
        openEHRDVFieldNames.put(OpenEHRDataValues.DV_TEXT, new String[]{OpenEHRDataValues.VALUE_ATT});
        openEHRDVClassName.put(OpenEHRDataValues.DV_TEXT, DvText.class.getSimpleName());
        openEHRDVIconName.put(OpenEHRDataValues.DV_TEXT, OpenEHRImageUtil.DV_TEXT_NAME);

        openEHRDVNames.put(OpenEHRDataValues.DV_CODED_TEXT, OpenEHRLanguageManager.getMessage("DataValueCodedText"));
        openEHRDVDescriptions.put(OpenEHRDataValues.DV_CODED_TEXT, OpenEHRLanguageManager.getMessage("DataValueCodedTextDesc"));
        openEHRDVIcons.put(OpenEHRDataValues.DV_CODED_TEXT, OpenEHRImageUtil.DV_CODED_TEXT_ICON);
        openEHRDVFieldNames.put(OpenEHRDataValues.DV_CODED_TEXT,
                new String[]{
                        OpenEHRDataValues.VALUE_ATT,
                        OpenEHRDataValues.TEMINOLOGYID_ATT,
                        OpenEHRDataValues.CODE_ATT});
        openEHRDVClassName.put(OpenEHRDataValues.DV_CODED_TEXT, DvCodedText.class.getSimpleName());
        openEHRDVIconName.put(OpenEHRDataValues.DV_CODED_TEXT, OpenEHRImageUtil.DV_CODED_TEXT_NAME);

        openEHRDVNames.put(OpenEHRDataValues.DV_ORDINAL, OpenEHRLanguageManager.getMessage("DataValueOrdinal"));
        openEHRDVDescriptions.put(OpenEHRDataValues.DV_ORDINAL, OpenEHRLanguageManager.getMessage("DataValueOrdinalDesc"));
        openEHRDVIcons.put(OpenEHRDataValues.DV_ORDINAL, OpenEHRImageUtil.DV_ORDINAL_ICON);
        openEHRDVFieldNames.put(OpenEHRDataValues.DV_ORDINAL,
                new String[]{
                        OpenEHRDataValues.VALUE_ATT,
                        OpenEHRDataValues.TEMINOLOGYID_ATT,
                        OpenEHRDataValues.SYMBOL_VALUE_ATT,
                        OpenEHRDataValues.CODE_ATT});
        openEHRDVClassName.put(OpenEHRDataValues.DV_ORDINAL, DvOrdinal.class.getSimpleName());
        openEHRDVIconName.put(OpenEHRDataValues.DV_ORDINAL, OpenEHRImageUtil.DV_ORDINAL_NAME);

        openEHRDVNames.put(OpenEHRDataValues.DV_QUANTITY, OpenEHRLanguageManager.getMessage("DataValueQuantity"));
        openEHRDVDescriptions.put(OpenEHRDataValues.DV_QUANTITY, OpenEHRLanguageManager.getMessage("DataValueQuantityDesc"));
        openEHRDVIcons.put(OpenEHRDataValues.DV_QUANTITY, OpenEHRImageUtil.DV_QUANTITY_ICON);
        openEHRDVFieldNames.put(OpenEHRDataValues.DV_QUANTITY,
                new String[]{
                        OpenEHRDataValues.MAGNITUDE_ATT,
                        OpenEHRDataValues.PRECISION_ATT,
                        OpenEHRDataValues.UNITS_ATT});
        openEHRDVClassName.put(OpenEHRDataValues.DV_QUANTITY, DvQuantity.class.getSimpleName());
        openEHRDVIconName.put(OpenEHRDataValues.DV_QUANTITY, OpenEHRImageUtil.DV_QUANTITY_NAME);

        openEHRDVNames.put(OpenEHRDataValues.DV_COUNT, OpenEHRLanguageManager.getMessage("DataValueCount"));
        openEHRDVDescriptions.put(OpenEHRDataValues.DV_COUNT, OpenEHRLanguageManager.getMessage("DataValueCountDesc"));
        openEHRDVIcons.put(OpenEHRDataValues.DV_COUNT, OpenEHRImageUtil.DV_COUNT_ICON);
        openEHRDVFieldNames.put(OpenEHRDataValues.DV_COUNT,
                new String[]{OpenEHRDataValues.MAGNITUDE_ATT});
        openEHRDVClassName.put(OpenEHRDataValues.DV_COUNT, DvCount.class.getSimpleName());
        openEHRDVIconName.put(OpenEHRDataValues.DV_COUNT, OpenEHRImageUtil.DV_COUNT_NAME);

        openEHRDVNames.put(OpenEHRDataValues.DV_DURATION, OpenEHRLanguageManager.getMessage("DataValueDuration"));
        openEHRDVDescriptions.put(OpenEHRDataValues.DV_DURATION, OpenEHRLanguageManager.getMessage("DataValueDurationDesc"));
        openEHRDVIcons.put(OpenEHRDataValues.DV_DURATION, OpenEHRImageUtil.DV_DURATION_ICON);
        openEHRDVFieldNames.put(OpenEHRDataValues.DV_DURATION,
                new String[]{OpenEHRDataValues.VALUE_ATT});
        openEHRDVClassName.put(OpenEHRDataValues.DV_DURATION, DvDuration.class.getSimpleName());
        openEHRDVIconName.put(OpenEHRDataValues.DV_DURATION, OpenEHRImageUtil.DV_DURATION_NAME);

        openEHRDVNames.put(OpenEHRDataValues.DV_DATE_TIME, OpenEHRLanguageManager.getMessage("DataValueDateTime"));
        openEHRDVDescriptions.put(OpenEHRDataValues.DV_DATE_TIME, OpenEHRLanguageManager.getMessage("DataValueDateTimeDesc"));
        openEHRDVIcons.put(OpenEHRDataValues.DV_DATE_TIME, OpenEHRImageUtil.DV_DATE_TIME_ICON);
        openEHRDVFieldNames.put(OpenEHRDataValues.DV_DATE_TIME,
                new String[]{
                        OpenEHRDataValues.YEAR_ATT,
                        OpenEHRDataValues.MONTH_ATT,
                        OpenEHRDataValues.DAY_ATT,
                        OpenEHRDataValues.HOUR_ATT,
                        OpenEHRDataValues.MINUTE_ATT,
                        OpenEHRDataValues.SECOND_ATT,
                        OpenEHRDataValues.FRACTIONAL_SECOND_ATT,
                        OpenEHRDataValues.TIMEZONE_ATT,
                        OpenEHRDataValues.VALUE_ATT});
        openEHRDVClassName.put(OpenEHRDataValues.DV_DATE_TIME, DvDateTime.class.getSimpleName());
        openEHRDVIconName.put(OpenEHRDataValues.DV_DATE_TIME, OpenEHRImageUtil.DV_DATE_TIME_NAME);

        openEHRDVNames.put(OpenEHRDataValues.DV_DATE, OpenEHRLanguageManager.getMessage("DataValueDate"));
        openEHRDVDescriptions.put(OpenEHRDataValues.DV_DATE, OpenEHRLanguageManager.getMessage("DataValueDateDesc"));
        openEHRDVIcons.put(OpenEHRDataValues.DV_DATE, OpenEHRImageUtil.DV_DATE_TIME_ICON);
        openEHRDVFieldNames.put(OpenEHRDataValues.DV_DATE,
                new String[]{
                        OpenEHRDataValues.YEAR_ATT,
                        OpenEHRDataValues.MONTH_ATT,
                        OpenEHRDataValues.DAY_ATT,
                        OpenEHRDataValues.VALUE_ATT});
        openEHRDVClassName.put(OpenEHRDataValues.DV_DATE, DvDate.class.getSimpleName());
        openEHRDVIconName.put(OpenEHRDataValues.DV_DATE, OpenEHRImageUtil.DV_DATE_TIME_NAME);

        openEHRDVNames.put(OpenEHRDataValues.DV_TIME, OpenEHRLanguageManager.getMessage("DataValueTime"));
        openEHRDVDescriptions.put(OpenEHRDataValues.DV_TIME, OpenEHRLanguageManager.getMessage("DataValueTimeDesc"));
        openEHRDVIcons.put(OpenEHRDataValues.DV_TIME, OpenEHRImageUtil.DV_DATE_TIME_ICON);
        openEHRDVFieldNames.put(OpenEHRDataValues.DV_TIME,
                new String[]{
                        OpenEHRDataValues.HOUR_ATT,
                        OpenEHRDataValues.MINUTE_ATT,
                        OpenEHRDataValues.SECOND_ATT,
                        OpenEHRDataValues.FRACTIONAL_SECOND_ATT,
                        OpenEHRDataValues.TIMEZONE_ATT,
                        OpenEHRDataValues.VALUE_ATT});
        openEHRDVClassName.put(OpenEHRDataValues.DV_TIME, DvTime.class.getSimpleName());
        openEHRDVIconName.put(OpenEHRDataValues.DV_TIME, OpenEHRImageUtil.DV_DATE_TIME_NAME);

        openEHRDVNames.put(OpenEHRDataValues.DV_BOOLEAN, OpenEHRLanguageManager.getMessage("DataValueBoolean"));
        openEHRDVDescriptions.put(OpenEHRDataValues.DV_BOOLEAN, OpenEHRLanguageManager.getMessage("DataValueBooleanDesc"));
        openEHRDVIcons.put(OpenEHRDataValues.DV_BOOLEAN, OpenEHRImageUtil.DV_BOOLEAN_ICON);
        openEHRDVFieldNames.put(OpenEHRDataValues.DV_BOOLEAN,
                new String[]{OpenEHRDataValues.VALUE_ATT});
        openEHRDVClassName.put(OpenEHRDataValues.DV_BOOLEAN, DvBoolean.class.getSimpleName());
        openEHRDVIconName.put(OpenEHRDataValues.DV_BOOLEAN, OpenEHRImageUtil.DV_BOOLEAN_NAME);

        openEHRDVNames.put(OpenEHRDataValues.DV_PROPORTION, OpenEHRLanguageManager.getMessage("DataValueProportion"));
        openEHRDVDescriptions.put(OpenEHRDataValues.DV_PROPORTION, OpenEHRLanguageManager.getMessage("DataValueProportionDesc"));
        openEHRDVIcons.put(OpenEHRDataValues.DV_PROPORTION, OpenEHRImageUtil.DV_PROPORTION_ICON);
        openEHRDVFieldNames.put(OpenEHRDataValues.DV_PROPORTION,
                new String[]{
                        OpenEHRDataValues.NUMERATOR_ATT,
                        OpenEHRDataValues.DENOMINATOR_ATT,
                        OpenEHRDataValues.TYPE_ATT,
                        OpenEHRDataValues.PRECISION_ATT});
        openEHRDVClassName.put(OpenEHRDataValues.DV_PROPORTION, DvProportion.class.getSimpleName());
        openEHRDVIconName.put(OpenEHRDataValues.DV_PROPORTION, OpenEHRImageUtil.DV_PROPORTION_NAME);

        openEHRDVNames.put(OpenEHRDataValues.DV_URI, OpenEHRLanguageManager.getMessage("DataValueURI"));
        openEHRDVDescriptions.put(OpenEHRDataValues.DV_URI, OpenEHRLanguageManager.getMessage("DataValueURIDesc"));
        openEHRDVIcons.put(OpenEHRDataValues.DV_URI, OpenEHRImageUtil.DV_URI_ICON);
        openEHRDVFieldNames.put(OpenEHRDataValues.DV_URI,
                new String[]{OpenEHRDataValues.VALUE_ATT});
        openEHRDVClassName.put(OpenEHRDataValues.DV_URI, DvURI.class.getSimpleName());
        openEHRDVIconName.put(OpenEHRDataValues.DV_URI, OpenEHRImageUtil.DV_URI_NAME);


        openEHRDVNames.put(OpenEHRDataValues.DV_PARSABLE, OpenEHRLanguageManager.getMessage("DataValueParsable"));
        openEHRDVDescriptions.put(OpenEHRDataValues.DV_PARSABLE, OpenEHRLanguageManager.getMessage("DataValueParsableDesc"));
        openEHRDVClassName.put(OpenEHRDataValues.DV_PARSABLE, DvParsable.class.getSimpleName());
        openEHRDVIconName.put(OpenEHRDataValues.DV_PARSABLE, OpenEHRImageUtil.DV_PARSABLE_NAME);
        openEHRDVIcons.put(OpenEHRDataValues.DV_PARSABLE, OpenEHRImageUtil.DV_PARSABLE_ICON);
        openEHRDVFieldNames.put(OpenEHRDataValues.DV_PARSABLE, new String[]{OpenEHRDataValues.FORMALISM_ATT, OpenEHRDataValues.VALUE_ATT});
    }

    public static String getName(String idDataValue) {
        return getDelegate().openEHRDVNames.get(idDataValue);
    }

    public static String getDescription(String idDataValue) {
        return getDelegate().openEHRDVDescriptions.get(idDataValue);
    }

    public static ImageIcon getIcon(String idDataValue) {
        return getDelegate().openEHRDVIcons.get(idDataValue);
    }

    public static Collection<String> getManagedDVs() {
        return getDelegate().openEHRDVNames.keySet();
    }

    public static String[] getFieldNames(String idDataValue) {
        String[] fieldNames = getDelegate().openEHRDVFieldNames.get(idDataValue);
        if (fieldNames != null) {
            return fieldNames;
        } else {
            return new String[0];
        }
    }

    public static ArrayList<String> getFunctionNames() {
        return GENERIC_FUNCTIONS;
    }

    public static String getDVClassName(String idDataValue) {
        return getDelegate().openEHRDVClassName.get(idDataValue);
    }

    public static String getDVIconName(String idDataValue) {
        return getDelegate().openEHRDVIconName.get(idDataValue);
    }

    public static CodePhrase getLanguageCodePhrase() {
        if (getDelegate()._languageCodePhrase == null) {
            getDelegate()._languageCodePhrase = OpenEHRConst.DEFAULT_LANGUAGE_CODE_PHRASE;
        }
        return getDelegate()._languageCodePhrase;
    }

    public static CodePhrase getDefaultLanguageCodePhrase() {
        if (getDelegate()._defaultLanguageCodePhrase == null) {
            getDelegate()._defaultLanguageCodePhrase = new CodePhrase(
                    "ISO_639-1",
                    "en");
        }
        return getDelegate()._defaultLanguageCodePhrase;
    }

    public static OpenEHRDataValuesUI getDelegate() {
        if (instance == null) {
            instance = new OpenEHRDataValuesUI();
        }
        return instance;
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