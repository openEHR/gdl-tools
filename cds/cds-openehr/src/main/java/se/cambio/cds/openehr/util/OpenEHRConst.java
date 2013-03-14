package se.cambio.cds.openehr.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.swing.ImageIcon;

import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;




public class OpenEHRConst {

    private static OpenEHRConst _instance = null;

    public final static String ELEMENT = "ELEMENT";
    public final static String CLUSTER = "CLUSTER";
    public final static String SECTION = "SECTION";
    public final static String STRUCTURE = "STRUCTURE";
    public final static String ACTIVITY = "ACTIVITY";
    public final static String ITEM_TREE = "ITEM_TREE";
    public final static String ITEM_LIST = "ITEM_LIST";
    public final static String ITEM_TABLE = "ITEM_TABLE";
    public final static String ITEM_SINGLE = "ITEM_SINGLE";
    public final static String PARTICIPATION = "PARTICIPATION";
    public final static String COMPOSITION = "COMPOSITION";
    public final static String POINT_EVENT = "POINT_EVENT";
    public final static String HISTORY = "HISTORY";
    
    //Entries
    public final static String OBSERVATION = "OBSERVATION";
    public final static String EVALUATION = "EVALUATION";
    public final static String INSTRUCTION = "INSTRUCTION";
    public final static String ACTION = "ACTION";

    //TODO Change
    public static String LOCAL = "local";
    public static String OPENEHR = "openehr";
    
    public static String NULL_FLAVOR_ATTRIBUTE = "null_flavor";
    public static DvCodedText NULL_FLAVOUR_CODE_NO_INFO = new DvCodedText("No information", new CodePhrase(OpenEHRConst.OPENEHR, "271")); //Default
    public static DvCodedText NULL_FLAVOUR_CODE_UNKNOWN = new DvCodedText("Unknown", new CodePhrase(OpenEHRConst.OPENEHR, "253"));
    public static DvCodedText NULL_FLAVOUR_CODE_MASKED = new DvCodedText("Masked", new CodePhrase(OpenEHRConst.OPENEHR, "272"));
    public static DvCodedText NULL_FLAVOUR_CODE_NOT_APPLICABLE = new DvCodedText("Not applicable", new CodePhrase(OpenEHRConst.OPENEHR, "273"));
    
    public static Map<String, DvCodedText> NULL_FLAVOUR_MAP = new LinkedHashMap<String, DvCodedText>();
    static {
	NULL_FLAVOUR_MAP.put("271", NULL_FLAVOUR_CODE_NO_INFO);
	NULL_FLAVOUR_MAP.put("253", NULL_FLAVOUR_CODE_UNKNOWN);
	NULL_FLAVOUR_MAP.put("272", NULL_FLAVOUR_CODE_MASKED);
	NULL_FLAVOUR_MAP.put("273", NULL_FLAVOUR_CODE_NOT_APPLICABLE);
    }

    public static String CURRENT_DATE_TIME_ID = "currentDateTime";
    
    private final HashMap<String,String> _openEHRConstNames;
    private final HashMap<String,String> _openEHRConstDescriptions;
    private final HashMap<String,ImageIcon> _openEHRConstIcons;
    private final HashMap<String,String> _openEHRConstIconNames;

    public static final Collection<String> PARSABLE_OPENEHR_RM_NAMES = new ArrayList<String>();
    static {
	PARSABLE_OPENEHR_RM_NAMES.add(ELEMENT);
	PARSABLE_OPENEHR_RM_NAMES.add(CLUSTER);
	PARSABLE_OPENEHR_RM_NAMES.add(SECTION);
	PARSABLE_OPENEHR_RM_NAMES.add(ACTIVITY);
	PARSABLE_OPENEHR_RM_NAMES.add(ITEM_TREE);
	PARSABLE_OPENEHR_RM_NAMES.add(ITEM_LIST);
	PARSABLE_OPENEHR_RM_NAMES.add(ITEM_TABLE);
	PARSABLE_OPENEHR_RM_NAMES.add(ITEM_SINGLE);
	PARSABLE_OPENEHR_RM_NAMES.add(COMPOSITION);
	PARSABLE_OPENEHR_RM_NAMES.add(OBSERVATION);
	PARSABLE_OPENEHR_RM_NAMES.add(EVALUATION);
	PARSABLE_OPENEHR_RM_NAMES.add(INSTRUCTION);
	PARSABLE_OPENEHR_RM_NAMES.add(ACTION);
	//PARSABLE_OPENEHR_RM_NAMES.add(POINT_EVENT);
	//PARSABLE_OPENEHR_RM_NAMES.add(HISTORY);
    }

    public static final Collection<String> ARCHETYPES_RM_NAMES = new ArrayList<String>();
    static {
	PARSABLE_OPENEHR_RM_NAMES.add(CLUSTER);
	PARSABLE_OPENEHR_RM_NAMES.add(ITEM_TREE);
	PARSABLE_OPENEHR_RM_NAMES.add(COMPOSITION);
	PARSABLE_OPENEHR_RM_NAMES.add(OBSERVATION);
	PARSABLE_OPENEHR_RM_NAMES.add(EVALUATION);
	PARSABLE_OPENEHR_RM_NAMES.add(INSTRUCTION);
	PARSABLE_OPENEHR_RM_NAMES.add(ACTION);
    }
    
    private OpenEHRConst(){
	_openEHRConstNames = new HashMap<String, String>();
	_openEHRConstDescriptions = new HashMap<String, String>();
	_openEHRConstIcons = new HashMap<String, ImageIcon>();
	_openEHRConstIconNames = new HashMap<String, String>();

	_openEHRConstNames.put(CLUSTER, OpenEHRLanguageManager.getMessage("Cluster"));
	_openEHRConstDescriptions.put(CLUSTER, OpenEHRLanguageManager.getMessage("ClusterDesc"));
	_openEHRConstIcons.put(CLUSTER, ImageUtil.CLUSTER);
	_openEHRConstIconNames.put(CLUSTER, ImageUtil.CLUSTER_NAME);

	_openEHRConstNames.put(SECTION, OpenEHRLanguageManager.getMessage("Section"));
	_openEHRConstDescriptions.put(SECTION, OpenEHRLanguageManager.getMessage("SectionDesc"));
	_openEHRConstIcons.put(SECTION, ImageUtil.SECTION);
	_openEHRConstIconNames.put(SECTION, ImageUtil.SECTION_NAME);
	
	_openEHRConstNames.put(ACTIVITY, OpenEHRLanguageManager.getMessage("Activity"));
	_openEHRConstDescriptions.put(ACTIVITY, OpenEHRLanguageManager.getMessage("ActivityDesc"));
	_openEHRConstIcons.put(ACTIVITY, ImageUtil.ACTIVITY);
	_openEHRConstIconNames.put(ACTIVITY, ImageUtil.ACTIVITY_NAME);

	_openEHRConstNames.put(STRUCTURE, OpenEHRLanguageManager.getMessage("Structure"));
	_openEHRConstDescriptions.put(STRUCTURE, OpenEHRLanguageManager.getMessage("StructureDesc"));
	_openEHRConstIcons.put(STRUCTURE, ImageUtil.STRUCTURE);
	_openEHRConstIconNames.put(STRUCTURE, ImageUtil.STRUCTURE_NAME);

	_openEHRConstNames.put(ITEM_TREE, OpenEHRLanguageManager.getMessage("ItemTree"));
	_openEHRConstDescriptions.put(ITEM_TREE, OpenEHRLanguageManager.getMessage("ItemTreeDesc"));
	_openEHRConstIcons.put(ITEM_TREE, ImageUtil.ITEM_TREE);
	_openEHRConstIconNames.put(ITEM_TREE, ImageUtil.ITEM_TREE_NAME);

	_openEHRConstNames.put(ITEM_LIST, OpenEHRLanguageManager.getMessage("ItemList"));
	_openEHRConstDescriptions.put(ITEM_LIST, OpenEHRLanguageManager.getMessage("ItemListDesc"));
	_openEHRConstIcons.put(ITEM_LIST, ImageUtil.ITEM_LIST);
	_openEHRConstIconNames.put(ITEM_LIST, ImageUtil.ITEM_LIST_NAME);
	
	_openEHRConstNames.put(ITEM_TABLE, OpenEHRLanguageManager.getMessage("ItemTable"));
	_openEHRConstDescriptions.put(ITEM_TABLE, OpenEHRLanguageManager.getMessage("ItemTableDesc"));
	_openEHRConstIcons.put(ITEM_TABLE, ImageUtil.ITEM_TABLE);
	_openEHRConstIconNames.put(ITEM_TABLE, ImageUtil.ITEM_TABLE_NAME);

	_openEHRConstNames.put(ITEM_SINGLE, OpenEHRLanguageManager.getMessage("ItemSingle"));
	_openEHRConstDescriptions.put(ITEM_SINGLE, OpenEHRLanguageManager.getMessage("ItemSingleDesc"));
	_openEHRConstIcons.put(ITEM_SINGLE, ImageUtil.ITEM_SINGLE);
	_openEHRConstIconNames.put(ITEM_SINGLE, ImageUtil.ITEM_SINGLE_NAME);

	
	_openEHRConstNames.put(ELEMENT, OpenEHRLanguageManager.getMessage("Element"));
	_openEHRConstDescriptions.put(ELEMENT, OpenEHRLanguageManager.getMessage("ElementDesc"));
	_openEHRConstIcons.put(ELEMENT, ImageUtil.ELEMENT);
	_openEHRConstIconNames.put(ELEMENT, ImageUtil.ELEMENT_NAME);

	_openEHRConstNames.put(COMPOSITION, OpenEHRLanguageManager.getMessage("Composition"));
	_openEHRConstDescriptions.put(COMPOSITION, OpenEHRLanguageManager.getMessage("CompositionDesc"));
	_openEHRConstIcons.put(COMPOSITION, ImageUtil.COMPOSITION_ICON);
	_openEHRConstIconNames.put(COMPOSITION, ImageUtil.COMPOSITION_NAME);

	//Entries

	_openEHRConstNames.put(OBSERVATION, OpenEHRLanguageManager.getMessage("EntryObservation"));
	_openEHRConstDescriptions.put(OBSERVATION, OpenEHRLanguageManager.getMessage("EntryObservationDesc"));
	_openEHRConstIcons.put(OBSERVATION, ImageUtil.ENTRY_OBSERVATION_ICON);
	_openEHRConstIconNames.put(OBSERVATION, ImageUtil.ENTRY_OBSERVATION_NAME);
	
	_openEHRConstNames.put(EVALUATION, OpenEHRLanguageManager.getMessage("EntryEvaluation"));
	_openEHRConstDescriptions.put(EVALUATION, OpenEHRLanguageManager.getMessage("EntryEvaluationDesc"));
	_openEHRConstIcons.put(EVALUATION, ImageUtil.ENTRY_EVALUATION_ICON);
	_openEHRConstIconNames.put(EVALUATION, ImageUtil.ENTRY_EVALUATION_NAME);
	
	_openEHRConstNames.put(INSTRUCTION, OpenEHRLanguageManager.getMessage("EntryInstruction"));
	_openEHRConstDescriptions.put(INSTRUCTION, OpenEHRLanguageManager.getMessage("EntryInstructionDesc"));
	_openEHRConstIcons.put(INSTRUCTION, ImageUtil.ENTRY_INSTRUCTION_ICON);
	_openEHRConstIconNames.put(INSTRUCTION, ImageUtil.ENTRY_INSTRUCTION_NAME);

	_openEHRConstNames.put(ACTION, OpenEHRLanguageManager.getMessage("EntryAction"));
	_openEHRConstDescriptions.put(ACTION, OpenEHRLanguageManager.getMessage("EntryActionDesc"));
	_openEHRConstIcons.put(ACTION, ImageUtil.ENTRY_ACTION_ICON);
	_openEHRConstIconNames.put(ACTION, ImageUtil.ENTRY_ACTION_NAME);
    }

    public static String getName(String idDataValue){
	return getDelegate()._openEHRConstNames.get(idDataValue);
    }

    public static String getDescription(String idDataValue){
	return getDelegate()._openEHRConstNames.get(idDataValue);
    }

    public static ImageIcon getIcon(String idDataValue){
	return getDelegate()._openEHRConstIcons.get(idDataValue);
    }
    public static String getIconName(String idDataValue){
 	return getDelegate()._openEHRConstIconNames.get(idDataValue);
     }
    public static OpenEHRConst getDelegate(){
	if (_instance == null){
	    _instance = new OpenEHRConst();
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