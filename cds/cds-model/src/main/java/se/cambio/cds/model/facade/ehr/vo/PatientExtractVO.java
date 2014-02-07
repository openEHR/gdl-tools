package se.cambio.cds.model.facade.ehr.vo;

import java.io.Serializable;
import java.util.Calendar;
import java.util.Collection;

public class PatientExtractVO implements Serializable{


    private static final long serialVersionUID = 16042012L;
    private String idPatient;
    private Calendar date;
    private Collection<ExtractItemVO> items;
    
    public PatientExtractVO(String idPatient, Calendar date, Collection<ExtractItemVO> items) {
	super();
	this.idPatient = idPatient;
	this.date = date;
	this.items = items;
    }

    public String getIdPatient() {
        return idPatient;
    }

    public void setIdPatient(String idPatient) {
        this.idPatient = idPatient;
    }

    public Calendar getDate() {
        return date;
    }

    public void setDate(Calendar date) {
        this.date = date;
    }

    public Collection<ExtractItemVO> getItems() {
        return items;
    }

    public void setItems(Collection<ExtractItemVO> items) {
        this.items = items;
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