package se.cambio.openehr.view.panels;

import com.toedter.calendar.JDateChooser;
import se.cambio.openehr.util.OpenEHRImageUtil;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;

public abstract class DVGenericDateTimePanel extends DVGenericPanel {

    private static final long serialVersionUID = 1L;
    private JDateChooser dateChooser;

    public DVGenericDateTimePanel(String idElement, String idTemplate, boolean allowNull, boolean requestFocus) {
        super(idElement, idTemplate, allowNull, requestFocus);
        this.setLayout(new BorderLayout());
        this.add(getDateChooser(), BorderLayout.CENTER);
    }

    public JDateChooser getDateChooser() {
        if (dateChooser == null) {
            dateChooser = new JDateChooser(getDateConstraints(), getCalendarBlanks(), '_');
            dateChooser.setIcon(OpenEHRImageUtil.CALENDAR_ICON);
            dateChooser.getCalendarButton().addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    dateChooser.getDateEditor().getUiComponent().requestFocus();
                }
            });
        }
        return dateChooser;
    }

    public Collection<JComponent> getJComponents() {
        Collection<JComponent> components = new ArrayList<JComponent>();
        components.add(getDateChooser());
        return components;
    }

    public abstract String getDateConstraints();

    public abstract String getCalendarBlanks();
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