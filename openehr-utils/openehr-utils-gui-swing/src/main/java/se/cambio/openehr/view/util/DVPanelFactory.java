package se.cambio.openehr.view.util;

import org.apache.log4j.Logger;

import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.view.panels.DVBooleanPanel;
import se.cambio.openehr.view.panels.DVCodedTextPanel;
import se.cambio.openehr.view.panels.DVCountPanel;
import se.cambio.openehr.view.panels.DVDatePanel;
import se.cambio.openehr.view.panels.DVDateTimePanel;
import se.cambio.openehr.view.panels.DVDurationPanel;
import se.cambio.openehr.view.panels.DVGenericPanel;
import se.cambio.openehr.view.panels.DVOrdinalPanel;
import se.cambio.openehr.view.panels.DVProportionPanel;
import se.cambio.openehr.view.panels.DVQuantityPanel;
import se.cambio.openehr.view.panels.DVTextPanel;
import se.cambio.openehr.view.panels.DVTimePanel;

public class DVPanelFactory {

    public static DVGenericPanel createDVPanel(String idElement, String idTemplate, String rmType, boolean allowNull, boolean enableUnits, boolean requestFocus){
        if (OpenEHRDataValues.DV_QUANTITY.equals(rmType)){
            return new DVQuantityPanel(idElement, idTemplate, allowNull, enableUnits, requestFocus);
        }else if (OpenEHRDataValues.DV_CODED_TEXT.equals(rmType)){
            return new DVCodedTextPanel(idElement, idTemplate, allowNull, requestFocus);
        }else if (OpenEHRDataValues.DV_BOOLEAN.equals(rmType)){
            return new DVBooleanPanel(idElement, idTemplate, allowNull, requestFocus);
        }else if (OpenEHRDataValues.DV_COUNT.equals(rmType)){
            return new DVCountPanel(idElement, idTemplate, allowNull, requestFocus);
        }else if (OpenEHRDataValues.DV_TIME.equals(rmType)){
            return new DVTimePanel(idElement, idTemplate, allowNull, requestFocus);
        }else if (OpenEHRDataValues.DV_DATE.equals(rmType)){
            return new DVDatePanel(idElement, idTemplate, allowNull, requestFocus);
        }else if (OpenEHRDataValues.DV_DATE_TIME.equals(rmType)){
            return new DVDateTimePanel(idElement, idTemplate, allowNull, requestFocus);
        }else if (OpenEHRDataValues.DV_DURATION.equals(rmType)){
            return new DVDurationPanel(idElement, idTemplate, allowNull, requestFocus);
        }else if (OpenEHRDataValues.DV_ORDINAL.equals(rmType)){
            return new DVOrdinalPanel(idElement, idTemplate, allowNull, requestFocus);
        }else if (OpenEHRDataValues.DV_PROPORTION.equals(rmType)){
            return new DVProportionPanel(idElement, idTemplate, allowNull, requestFocus);
        }else if (OpenEHRDataValues.DV_TEXT.equals(rmType)){
            return new DVTextPanel(idElement, idTemplate, allowNull, requestFocus);
        }else{
            //TODO Types left ??
            Logger.getLogger(DVPanelFactory.class).error("Unknown rmType '"+rmType+"'");
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