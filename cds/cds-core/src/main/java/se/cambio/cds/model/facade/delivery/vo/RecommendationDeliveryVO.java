package se.cambio.cds.model.facade.delivery.vo;

public class RecommendationDeliveryVO {

	private String deliveryChannelId;

	private String userId;

	private String addresseeId;

	private String msg;

	public RecommendationDeliveryVO(String deliveryChannelId,
		String userId, String addresseeId, String msg) {
	    super();
	    this.deliveryChannelId = deliveryChannelId;
	    this.userId = userId;
	    this.addresseeId = addresseeId;
	    this.msg = msg;
	}

	public String getDeliveryChannelId() {
	    return deliveryChannelId;
	}

	public void setDeliveryChannelId(String deliveryChannelId) {
	    this.deliveryChannelId = deliveryChannelId;
	}

	public String getUserId() {
	    return userId;
	}

	public void setUserId(String userId) {
	    this.userId = userId;
	}

	public String getAddresseeId() {
	    return addresseeId;
	}

	public void setAddresseeId(String addresseeId) {
	    this.addresseeId = addresseeId;
	}

	public String getMsg() {
	    return msg;
	}

	public void setMsg(String msg) {
	    this.msg = msg;
	}
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  1.1 (the 'License'); you may not use this file except in compliance with
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