package se.cambio.cds.openehr.view.applicationobjects;


import javax.swing.ImageIcon;

import se.cambio.cds.openehr.util.ImageUtil;
import se.cambio.cds.openehr.util.OpenEHRLanguageManager;
import se.cambio.cds.util.AggregationFunctions;

public class AggregationFunctionsUI {

    public static String LAST_NAME = OpenEHRLanguageManager.getMessage("Last");

    public static String getAggregationFunctionName(String aggregationFunction){
	if (AggregationFunctions.ID_AGGREGATION_FUNCTION_LAST.equals(aggregationFunction)){
	    return LAST_NAME;
	}
	return null;
    }

    public static ImageIcon getIcon(String af){
	if (af==null){
	    return ImageUtil.AF_ALL_ICON;
	}else if(AggregationFunctions.ID_AGGREGATION_FUNCTION_LAST.equals(af)){
	    return ImageUtil.AF_LAST_ICON;
	}else if(AggregationFunctions.ID_AGGREGATION_FUNCTION_DURATION.equals(af)){
	    return ImageUtil.AF_DURATION_ICON;
	}else{
	    return null;
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