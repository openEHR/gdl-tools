package se.cambio.openehr.util;

import java.util.ArrayList;
import java.util.Collection;




public class OpenEHRConst {

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
    public final static String EVENT = "EVENT";

    //Entries
    public final static String OBSERVATION = "OBSERVATION";
    public final static String EVALUATION = "EVALUATION";
    public final static String INSTRUCTION = "INSTRUCTION";
    public final static String ACTION = "ACTION";

    public final static String ITEMS = "items";
    public final static String DATA = "data";
    public final static String VALUE = "value";
    public final static String DEFINING_CODE = "defining_code";


    //TODO Change
    public static String LOCAL = "local";
    public static String OPENEHR = "openehr";

    public static String NULL_FLAVOR_ATTRIBUTE = "null_flavor";



    public static String CURRENT_DATE_TIME_ID = "currentDateTime";

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