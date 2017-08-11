package se.cambio.openehr.view.panels;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.datetime.DvTime;
import se.cambio.openehr.view.util.DVConverter;

import java.util.Calendar;

public class DVTimePanel extends DVGenericDateTimePanel implements DVPanelInterface {

    private static final long serialVersionUID = 1L;

    public DVTimePanel(String idElement, String idTemplate, boolean allowNull, boolean requestFocus) {
        super(idElement, idTemplate, allowNull, requestFocus);
    }

    public void setDataValue(DataValue dataValue) {
        Calendar cal = null;
        if (dataValue instanceof DvTime) {
            cal = Calendar.getInstance();
            DvTime dv = (DvTime) dataValue;
            cal.setTime(dv.getDateTime().toDate());
        }
        getDateChooser().setCalendar(cal);

    }

    public DataValue getDataValue() {
        Calendar cal = getDateChooser().getCalendar();
        return DVConverter.getDvTime(cal);
    }

    public String getDateConstraints() {
        return "HH:mm:ss";
    }

    public String getCalendarBlanks() {
        return "##:##:##";
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