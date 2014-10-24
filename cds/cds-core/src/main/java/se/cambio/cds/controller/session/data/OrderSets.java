package se.cambio.cds.controller.session.data;

import se.cambio.openehr.controller.session.data.AbstractCMManager;
import se.cambio.cm.model.orderset.dto.OrderSetDTO;


public class OrderSets extends AbstractCMManager<OrderSetDTO> {
    public static OrderSets _instance = null;

    public OrderSets(){
    }

    @Override
    public Class<OrderSetDTO> getCMElementClass() {
        return OrderSetDTO.class;
    }

    public static OrderSets getInstance(){
        if (_instance == null){
            _instance = new OrderSets();
        }
        return _instance;
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