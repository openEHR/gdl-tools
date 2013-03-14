package se.cambio.cds.model.recommendation.dto;

import java.io.Serializable;
import java.util.Calendar;

public class RecommendationDTO implements Serializable {
    

    	private static final long serialVersionUID = 20120412L;
	private Long idRecommendation;
	private String idRule;
	private String idPatient;
	private Integer discardRepeatsPeriod;
	private String idUserGroup;
	private String idUser;
	private String idChannel;
	private String address;
	/**
	 * level
	 * 0 - NO CONFIRMATION REQUIRED
	 * 1 - CONFIRM BEFORE 30 MIN
	 * 2 - CONFIRM BEFORE CURRENT TURN ENDS
	 * 3 - CONFIRM BEFORE 24 HOURS
	 * 4 - CONFIRM BEFORE NEXT TURN ENDS
	 */
	private Short level;
	private String msg;
	private Calendar fireDate;
	/**
	 * state
	 * 0 - SENT
	 * 1 - CONFIRMED
	 * 2 - ESCALATED ON TIMEOUT
	 * 3 - ESCALATED ON USER GROUP EMPTY
	 * 4 - LOST ON DELIVERY
	 * 5 - PATIENT FILTERED
	 */
	private Short state;
	private Calendar responseDate;
	private String response;
	
	public RecommendationDTO(Long idRecommendation, String idRule,
		    String idPatient, Integer discardRepeatsPeriod, String idUserGroup,
		    String idUser, String idChannel, String address,
		    Short level, String msg, Calendar fireDate, Short state,
		    Calendar responseDate, String response) {
		super();
		this.idRecommendation = idRecommendation;
		this.idRule = idRule;
		this.idPatient = idPatient;
		this.discardRepeatsPeriod = discardRepeatsPeriod;
		this.idUserGroup = idUserGroup;
		this.idUser = idUser;
		this.idChannel = idChannel;
		this.address = address;
		this.level = level;
		this.msg = msg;
		this.fireDate = fireDate;
		this.state = state;
		this.responseDate = responseDate;
		this.response = response;
	    }

	public Long getIdRecommendation() {
	    return idRecommendation;
	}

	public void setIdRecommendation(Long idRecommendation) {
	    this.idRecommendation = idRecommendation;
	}

	public String getIdRule() {
	    return idRule;
	}

	public void setIdRule(String idRule) {
	    this.idRule = idRule;
	}

	public String getIdPatient() {
	    return idPatient;
	}

	public void setIdPatient(String idPatient) {
	    this.idPatient = idPatient;
	}

	public Integer getDiscardRepeatsPeriod() {
	    return discardRepeatsPeriod;
	}

	public void setDiscardRepeatsPeriod(Integer discardRepeatsPeriod) {
	    this.discardRepeatsPeriod = discardRepeatsPeriod;
	}

	public String getIdUserGroup() {
	    return idUserGroup;
	}

	public void setIdUserGroup(String idUserGroup) {
	    this.idUserGroup = idUserGroup;
	}

	public String getIdUser() {
	    return idUser;
	}

	public void setIdUser(String idUser) {
	    this.idUser = idUser;
	}

	public String getIdDecisionDeliveryChannel() {
	    return idChannel;
	}

	public void setIdChannel(String idChannel) {
	    this.idChannel = idChannel;
	}

	public String getAddress() {
	    return address;
	}

	public void setAddress(String address) {
	    this.address = address;
	}

	public Short getLevel() {
	    return level;
	}

	public void setLevel(Short level) {
	    this.level = level;
	}

	public String getMsg() {
	    return msg;
	}

	public void setMsg(String msg) {
	    this.msg = msg;
	}

	public Calendar getFireDate() {
	    return fireDate;
	}

	public void setFireDate(Calendar fireDate) {
	    this.fireDate = fireDate;
	}


	public Short getState() {
	    return state;
	}

	public void setState(Short state) {
	    this.state = state;
	}

	public Calendar getResponseDate() {
	    return responseDate;
	}

	public void setResponseDate(Calendar responseDate) {
	    this.responseDate = responseDate;
	}
	
	public String getResponse() {
	    return response;
	}

	public void setDecisionResponse(String response) {
	    this.response = response;
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