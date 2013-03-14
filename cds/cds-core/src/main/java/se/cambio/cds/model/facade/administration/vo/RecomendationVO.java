package se.cambio.cds.model.facade.administration.vo;

import java.util.Calendar;

public class RecomendationVO {

	private Long idDeliveredRecommendation;

	private String idRule;

	private String idPatient;

	private Integer discardRepeatsPeriod;

	private String idUserGroup;

	private String idUser;

	private String idDecisionDeliveryChannel;

	private String address;

	private Short level;

	private String msg;

	private Calendar fireDate;

	private Calendar storeDate;

	/**
	 * 0 - SENT
	 * 1 - CONFIRMED
	 * 2 - ESCALATED ON TIMEOUT
	 * 3 - ESCALATED ON USER GROUP EMPTY
	 * 4 - LOST ON DELIVERY
	 * 5 - PATIENT FILTERED
	 */
	private Short state;

	private String decisionResponse;

	public Long getIdDeliveredRecommendation() {
	    return idDeliveredRecommendation;
	}

	public void setIdDeliveredRecommendation(Long idDeliveredRecommendation) {
	    this.idDeliveredRecommendation = idDeliveredRecommendation;
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
	    return idDecisionDeliveryChannel;
	}

	public void setIdDecisionDeliveryChannel(String idDecisionDeliveryChannel) {
	    this.idDecisionDeliveryChannel = idDecisionDeliveryChannel;
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

	public Calendar getStoreDate() {
	    return storeDate;
	}

	public void setStoreDate(Calendar storeDate) {
	    this.storeDate = storeDate;
	}

	public Short getState() {
	    return state;
	}

	public void setState(Short state) {
	    this.state = state;
	}

	public String getDecisionResponse() {
	    return decisionResponse;
	}

	public void setDecisionResponse(String decisionResponse) {
	    this.decisionResponse = decisionResponse;
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