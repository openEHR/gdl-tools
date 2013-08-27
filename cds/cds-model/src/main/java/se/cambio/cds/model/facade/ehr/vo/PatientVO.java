package se.cambio.cds.model.facade.ehr.vo;

import java.io.Serializable;
import java.util.Calendar;

public class PatientVO implements Serializable{


    private static final long serialVersionUID = 16042012L;
    private String idPatient;
    private Calendar born;
    private Character gender;
    private Short state;
    private Calendar stateDate;

    public PatientVO(String idPatient, Calendar born, Character gender,
	    Short state, Calendar stateDate) {
	super();
	this.idPatient = idPatient;
	this.born = born;
	this.gender = gender;
	this.state = state;
	this.stateDate = stateDate;
    }

    public String getIdPatient() {
	return idPatient;
    }

    public void setIdPatient(String idPatient) {
	this.idPatient = idPatient;
    }

    public Calendar getBorn() {
	return born;
    }

    public void setBorn(Calendar born) {
	this.born = born;
    }

    public Character getGender() {
	return gender;
    }

    public void setGender(Character gender) {
	this.gender = gender;
    }

    public Short getState() {
	return state;
    }

    public void setState(Short state) {
	this.state = state;
    }

    public Calendar getStateDate() {
	return stateDate;
    }

    public void setStateDate(Calendar stateDate) {
	this.stateDate = stateDate;
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