package se.cambio.openehr.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

import javax.swing.ImageIcon;

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

import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.UserConfigurationManager;

/**
 * @author iago.corbal
 *
 */
public class OpenEHRDataValuesUI {

    private static OpenEHRDataValuesUI _instance = null;
    private final HashMap<String,String> _openEHRDVNames;
    private final HashMap<String,String> _openEHRDVDescriptions;
    private final HashMap<String,ImageIcon> _openEHRDVIcons;
    private final HashMap<String,String[]> _openEHRDVFieldNames;
    private final HashMap<String,String> _openEHRDVClassName;
    private final HashMap<String,String> _openEHRDVIconName;

    private static final ArrayList<String> GENERIC_FUNCTIONS = new ArrayList<String>();
    static{
	GENERIC_FUNCTIONS.add(OpenEHRDataValues.FUNCTION_COUNT);
    }

    private CodePhrase _languageCodePhrase = null;
    private CodePhrase _defaultLanguageCodePhrase = null;

    private OpenEHRDataValuesUI(){
	_openEHRDVNames = new HashMap<String, String>();
	_openEHRDVDescriptions = new HashMap<String, String>();
	_openEHRDVIcons = new HashMap<String, ImageIcon>();
	_openEHRDVFieldNames = new HashMap<String, String[]>();
	_openEHRDVClassName = new HashMap<String, String>();
	_openEHRDVIconName = new HashMap<String, String>();

	_openEHRDVNames.put(OpenEHRDataValues.DV_TEXT, OpenEHRLanguageManager.getMessage("DataValueText"));
	_openEHRDVDescriptions.put(OpenEHRDataValues.DV_TEXT, OpenEHRLanguageManager.getMessage("DataValueTextDesc"));
	_openEHRDVIcons.put(OpenEHRDataValues.DV_TEXT, OpenEHRImageUtil.DV_TEXT_ICON);
	_openEHRDVFieldNames.put(OpenEHRDataValues.DV_TEXT, 
		new String[]{OpenEHRDataValues.VALUE_ATT});
	_openEHRDVClassName.put(OpenEHRDataValues.DV_TEXT, DvText.class.getSimpleName());
	_openEHRDVIconName.put(OpenEHRDataValues.DV_TEXT, OpenEHRImageUtil.DV_TEXT_NAME);

	_openEHRDVNames.put(OpenEHRDataValues.DV_CODED_TEXT, OpenEHRLanguageManager.getMessage("DataValueCodedText"));
	_openEHRDVDescriptions.put(OpenEHRDataValues.DV_CODED_TEXT, OpenEHRLanguageManager.getMessage("DataValueCodedTextDesc"));
	_openEHRDVIcons.put(OpenEHRDataValues.DV_CODED_TEXT, OpenEHRImageUtil.DV_CODED_TEXT_ICON);
	_openEHRDVFieldNames.put(OpenEHRDataValues.DV_CODED_TEXT, 
		new String[]{
		OpenEHRDataValues.VALUE_ATT, 
		OpenEHRDataValues.TEMINOLOGYID_ATT, 
		OpenEHRDataValues.CODE_ATT});
	_openEHRDVClassName.put(OpenEHRDataValues.DV_CODED_TEXT, DvCodedText.class.getSimpleName());
	_openEHRDVIconName.put(OpenEHRDataValues.DV_CODED_TEXT, OpenEHRImageUtil.DV_CODED_TEXT_NAME);

	_openEHRDVNames.put(OpenEHRDataValues.DV_ORDINAL, OpenEHRLanguageManager.getMessage("DataValueOrdinal"));
	_openEHRDVDescriptions.put(OpenEHRDataValues.DV_ORDINAL, OpenEHRLanguageManager.getMessage("DataValueOrdinalDesc"));
	_openEHRDVIcons.put(OpenEHRDataValues.DV_ORDINAL, OpenEHRImageUtil.DV_ORDINAL_ICON);
	_openEHRDVFieldNames.put(OpenEHRDataValues.DV_ORDINAL, 
		new String[]{
		OpenEHRDataValues.VALUE_ATT,
		OpenEHRDataValues.TEMINOLOGYID_ATT, 
		OpenEHRDataValues.CODE_ATT});
	_openEHRDVClassName.put(OpenEHRDataValues.DV_ORDINAL, DvOrdinal.class.getSimpleName());
	_openEHRDVIconName.put(OpenEHRDataValues.DV_ORDINAL, OpenEHRImageUtil.DV_ORDINAL_NAME);

	_openEHRDVNames.put(OpenEHRDataValues.DV_QUANTITY, OpenEHRLanguageManager.getMessage("DataValueQuantity"));
	_openEHRDVDescriptions.put(OpenEHRDataValues.DV_QUANTITY, OpenEHRLanguageManager.getMessage("DataValueQuantityDesc"));
	_openEHRDVIcons.put(OpenEHRDataValues.DV_QUANTITY, OpenEHRImageUtil.DV_QUANTITY_ICON);
	_openEHRDVFieldNames.put(OpenEHRDataValues.DV_QUANTITY, 
		new String[]{
		OpenEHRDataValues.MAGNITUDE_ATT, 
		OpenEHRDataValues.PRECISION_ATT, 
		OpenEHRDataValues.UNITS_ATT});
	_openEHRDVClassName.put(OpenEHRDataValues.DV_QUANTITY, DvQuantity.class.getSimpleName());
	_openEHRDVIconName.put(OpenEHRDataValues.DV_QUANTITY, OpenEHRImageUtil.DV_QUANTITY_NAME);

	_openEHRDVNames.put(OpenEHRDataValues.DV_COUNT, OpenEHRLanguageManager.getMessage("DataValueCount"));
	_openEHRDVDescriptions.put(OpenEHRDataValues.DV_COUNT, OpenEHRLanguageManager.getMessage("DataValueCountDesc"));
	_openEHRDVIcons.put(OpenEHRDataValues.DV_COUNT, OpenEHRImageUtil.DV_COUNT_ICON);
	_openEHRDVFieldNames.put(OpenEHRDataValues.DV_COUNT, 
		new String[]{OpenEHRDataValues.MAGNITUDE_ATT});
	_openEHRDVClassName.put(OpenEHRDataValues.DV_COUNT, DvCount.class.getSimpleName());
	_openEHRDVIconName.put(OpenEHRDataValues.DV_COUNT, OpenEHRImageUtil.DV_COUNT_NAME);

	_openEHRDVNames.put(OpenEHRDataValues.DV_DURATION, OpenEHRLanguageManager.getMessage("DataValueDuration"));
	_openEHRDVDescriptions.put(OpenEHRDataValues.DV_DURATION, OpenEHRLanguageManager.getMessage("DataValueDurationDesc"));
	_openEHRDVIcons.put(OpenEHRDataValues.DV_DURATION, OpenEHRImageUtil.DV_DURATION_ICON);
	_openEHRDVFieldNames.put(OpenEHRDataValues.DV_DURATION, 
		new String[]{OpenEHRDataValues.VALUE_ATT});
	_openEHRDVClassName.put(OpenEHRDataValues.DV_DURATION, DvDuration.class.getSimpleName());
	_openEHRDVIconName.put(OpenEHRDataValues.DV_DURATION, OpenEHRImageUtil.DV_DURATION_NAME);

	_openEHRDVNames.put(OpenEHRDataValues.DV_DATE_TIME, OpenEHRLanguageManager.getMessage("DataValueDateTime"));
	_openEHRDVDescriptions.put(OpenEHRDataValues.DV_DATE_TIME, OpenEHRLanguageManager.getMessage("DataValueDateTimeDesc"));
	_openEHRDVIcons.put(OpenEHRDataValues.DV_DATE_TIME, OpenEHRImageUtil.DV_DATE_TIME_ICON);
	_openEHRDVFieldNames.put(OpenEHRDataValues.DV_DATE_TIME, 
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
	_openEHRDVClassName.put(OpenEHRDataValues.DV_DATE_TIME, DvDateTime.class.getSimpleName());
	_openEHRDVIconName.put(OpenEHRDataValues.DV_DATE_TIME, OpenEHRImageUtil.DV_DATE_TIME_NAME);

	_openEHRDVNames.put(OpenEHRDataValues.DV_DATE, OpenEHRLanguageManager.getMessage("DataValueDate"));
	_openEHRDVDescriptions.put(OpenEHRDataValues.DV_DATE, OpenEHRLanguageManager.getMessage("DataValueDateDesc"));
	_openEHRDVIcons.put(OpenEHRDataValues.DV_DATE, OpenEHRImageUtil.DV_DATE_TIME_ICON);
	_openEHRDVFieldNames.put(OpenEHRDataValues.DV_DATE,
		new String[]{
		OpenEHRDataValues.YEAR_ATT, 
		OpenEHRDataValues.MONTH_ATT, 
		OpenEHRDataValues.DAY_ATT, 
		OpenEHRDataValues.VALUE_ATT});
	_openEHRDVClassName.put(OpenEHRDataValues.DV_DATE, DvDate.class.getSimpleName());
	_openEHRDVIconName.put(OpenEHRDataValues.DV_DATE, OpenEHRImageUtil.DV_DATE_TIME_NAME);

	_openEHRDVNames.put(OpenEHRDataValues.DV_TIME, OpenEHRLanguageManager.getMessage("DataValueTime"));
	_openEHRDVDescriptions.put(OpenEHRDataValues.DV_TIME, OpenEHRLanguageManager.getMessage("DataValueTimeDesc"));
	_openEHRDVIcons.put(OpenEHRDataValues.DV_TIME, OpenEHRImageUtil.DV_DATE_TIME_ICON);
	_openEHRDVFieldNames.put(OpenEHRDataValues.DV_TIME, 
		new String[]{
		OpenEHRDataValues.HOUR_ATT, 
		OpenEHRDataValues.MINUTE_ATT, 
		OpenEHRDataValues.SECOND_ATT, 
		OpenEHRDataValues.FRACTIONAL_SECOND_ATT, 
		OpenEHRDataValues.TIMEZONE_ATT, 
		OpenEHRDataValues.VALUE_ATT});
	_openEHRDVClassName.put(OpenEHRDataValues.DV_TIME, DvTime.class.getSimpleName());
	_openEHRDVIconName.put(OpenEHRDataValues.DV_TIME, OpenEHRImageUtil.DV_DATE_TIME_NAME);

	_openEHRDVNames.put(OpenEHRDataValues.DV_BOOLEAN, OpenEHRLanguageManager.getMessage("DataValueBoolean"));
	_openEHRDVDescriptions.put(OpenEHRDataValues.DV_BOOLEAN, OpenEHRLanguageManager.getMessage("DataValueBooleanDesc"));
	_openEHRDVIcons.put(OpenEHRDataValues.DV_BOOLEAN, OpenEHRImageUtil.DV_BOOLEAN_ICON);
	_openEHRDVFieldNames.put(OpenEHRDataValues.DV_BOOLEAN, 
		new String[]{OpenEHRDataValues.VALUE_ATT});
	_openEHRDVClassName.put(OpenEHRDataValues.DV_BOOLEAN, DvBoolean.class.getSimpleName());
	_openEHRDVIconName.put(OpenEHRDataValues.DV_BOOLEAN, OpenEHRImageUtil.DV_BOOLEAN_NAME);

	_openEHRDVNames.put(OpenEHRDataValues.DV_PROPORTION, OpenEHRLanguageManager.getMessage("DataValueProportion"));
	_openEHRDVDescriptions.put(OpenEHRDataValues.DV_PROPORTION, OpenEHRLanguageManager.getMessage("DataValueProportionDesc"));
	_openEHRDVIcons.put(OpenEHRDataValues.DV_PROPORTION, OpenEHRImageUtil.DV_PROPORTION_ICON);
	_openEHRDVFieldNames.put(OpenEHRDataValues.DV_PROPORTION, 
		new String[]{
		OpenEHRDataValues.NUMERATOR_ATT, 
		OpenEHRDataValues.DENOMINATOR_ATT, 
		OpenEHRDataValues.TYPE_ATT, 
		OpenEHRDataValues.PRECISION_ATT});
	_openEHRDVClassName.put(OpenEHRDataValues.DV_PROPORTION, DvProportion.class.getSimpleName());
	_openEHRDVIconName.put(OpenEHRDataValues.DV_PROPORTION, OpenEHRImageUtil.DV_PROPORTION_NAME);
    }

    public static String getName(String idDataValue){
	return getDelegate()._openEHRDVNames.get(idDataValue);
    }

    public static String getDescription(String idDataValue){
	return getDelegate()._openEHRDVDescriptions.get(idDataValue);
    }

    public static ImageIcon getIcon(String idDataValue){
	return getDelegate()._openEHRDVIcons.get(idDataValue);
    }

    public static boolean isManaged(String idDataValue){
	return getDelegate()._openEHRDVNames.containsKey(idDataValue);
    }

    public static Collection<String> getManagedDVs(){
	return getDelegate()._openEHRDVNames.keySet();
    }

    public static String[] getFieldNames(String idDataValue){
	return getDelegate()._openEHRDVFieldNames.get(idDataValue);
    }

    public static ArrayList<String> getFunctionNames(){
	return GENERIC_FUNCTIONS;
    }

    public static String getDVClassName(String idDataValue){
	return getDelegate()._openEHRDVClassName.get(idDataValue);
    }

    public static String getDVIconName(String idDataValue){
	return getDelegate()._openEHRDVIconName.get(idDataValue);
    }

    public static CodePhrase getLanguageCodePhrase(){
	if (getDelegate()._languageCodePhrase==null){
	    getDelegate()._languageCodePhrase = new CodePhrase(
		    "ISO_639-1", 
		    UserConfigurationManager.getLanguage());
	}
	return getDelegate()._languageCodePhrase;
    }
    
    public static CodePhrase getDefaultLanguageCodePhrase(){
	if (getDelegate()._defaultLanguageCodePhrase==null){
	    getDelegate()._defaultLanguageCodePhrase = new CodePhrase(
		    "ISO_639-1", 
		    UserConfigurationManager.getLanguage());
	}
	return getDelegate()._defaultLanguageCodePhrase;
    }
    
    public static OpenEHRDataValuesUI getDelegate(){
	if (_instance == null){
	    _instance = new OpenEHRDataValuesUI();
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