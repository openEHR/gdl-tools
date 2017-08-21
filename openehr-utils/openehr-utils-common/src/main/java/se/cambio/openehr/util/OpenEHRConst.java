package se.cambio.openehr.util;

import org.openehr.rm.datatypes.text.CodePhrase;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;


public class OpenEHRConst {

    public static final String ELEMENT = "ELEMENT";
    public static final String CLUSTER = "CLUSTER";
    public static final String SECTION = "SECTION";
    public static final String STRUCTURE = "STRUCTURE";
    public static final String ACTIVITY = "ACTIVITY";
    public static final String ITEM_TREE = "ITEM_TREE";
    public static final String ITEM_LIST = "ITEM_LIST";
    public static final String ITEM_TABLE = "ITEM_TABLE";
    public static final String ITEM_SINGLE = "ITEM_SINGLE";
    public static final String PARTICIPATION = "PARTICIPATION";
    public static final String COMPOSITION = "COMPOSITION";
    public static final String POINT_EVENT = "POINT_EVENT";
    public static final String HISTORY = "HISTORY";
    public static final String EVENT = "EVENT";

    //Entries
    public static final String OBSERVATION = "OBSERVATION";
    public static final String EVALUATION = "EVALUATION";
    public static final String INSTRUCTION = "INSTRUCTION";
    public static final String ACTION = "ACTION";
    public static final String ENTRY = "ENTRY";

    public static final String ITEMS = "items";
    public static final String DATA = "data";
    public static final String VALUE = "value";
    public static final String DEFINING_CODE = "defining_code";


    //TODO Change
    public static String LOCAL = "local";
    public static String OPENEHR = "openehr";

    public static String NULL_FLAVOR_ATTRIBUTE = "null_flavor";


    public static CodePhrase DEFAULT_LANGUAGE_CODE_PHRASE = new CodePhrase("ISO_639-1", "en");


    public static String CURRENT_DATE_TIME_ID = "currentDateTime";

    public static final Set<String> PARSABLE_OPENEHR_RM_NAMES = new HashSet<>();
    public static final Set<String> ENTRY_RM_NAMES = new HashSet<>();

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
        PARSABLE_OPENEHR_RM_NAMES.add(POINT_EVENT);
        PARSABLE_OPENEHR_RM_NAMES.add(HISTORY);
        PARSABLE_OPENEHR_RM_NAMES.add(EVENT);
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

        ENTRY_RM_NAMES.add(OBSERVATION);
        ENTRY_RM_NAMES.add(EVALUATION);
        ENTRY_RM_NAMES.add(INSTRUCTION);
        ENTRY_RM_NAMES.add(ACTION);
    }

    public static boolean isEntry(String rmName) {
        return ENTRY_RM_NAMES.contains(rmName);
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