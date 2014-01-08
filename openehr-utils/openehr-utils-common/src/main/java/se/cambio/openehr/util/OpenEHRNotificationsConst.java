package se.cambio.openehr.util;


public class OpenEHRNotificationsConst {

    public static String OPENEHR_ALERT_ARCHETYPE_ID = "openEHR-EHR-EVALUATION.alert.v1";
    public static String OPENEHR_ALERT_ELEMENT_ID = OPENEHR_ALERT_ARCHETYPE_ID+"/data[at0001]/items[at0015]";
    public static String OPENEHR_ALERT_STATUS_ELEMENT_ID = OPENEHR_ALERT_ARCHETYPE_ID+"/data[at0001]/items[at0009]";
    public static String OPENEHR_ALERT_CATEGORY_ELEMENT_ID = OPENEHR_ALERT_ARCHETYPE_ID+"/data[at0001]/items[at0002]";
    
    public static String OPENEHR_RISK_ARCHETYPE_ID = "openEHR-EHR-EVALUATION.risk.v1";
    public static String OPENEHR_RISK_ELEMENT_ID = OPENEHR_RISK_ARCHETYPE_ID+"/data[at0001]/items[at0002]";
    public static String OPENEHR_RISK_SIGNIFICANCE_ID = OPENEHR_RISK_ARCHETYPE_ID+"/data[at0001]/items[at0003]";

    public static String ALERT_CATEGORY_HIGH_CODE = "HIGH";
    public static String ALERT_CATEGORY_MEDIUM_CODE = "MEDIUM";
    public static String ALERT_CATEGORY_LOW_CODE = "LOW";

    public static String ALERT_ACTIVE_STATUS_CODE = "at0011";

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