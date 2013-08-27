package se.cambio.openehr.view.panels;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.DvOrdinal;

import se.cambio.openehr.controller.session.data.Ordinals;
import se.cambio.openehr.model.archetype.vo.OrdinalVO;

public class DVOrdinalPanel extends DVComboBoxPanel implements DVPanelInterface{

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    public DVOrdinalPanel(String idElement, String idTemplate, boolean allowNull, boolean requestFocus){
        super(idElement, idTemplate, allowNull, requestFocus);
        for (OrdinalVO ordinalVO : Ordinals.getOrdinalVOs(idTemplate, idElement)) {
            insertOption(""+ordinalVO.getValue(), ordinalVO.getValue()+" - "+ordinalVO.getName(), ordinalVO.getDescription());
        }
    }

    public void setDataValue(DataValue dataValue) {
        String code = null;
        if (dataValue instanceof DvOrdinal){
            code =""+((DvOrdinal)dataValue).getValue();
        }
        getComboBox().setSelectedItem(code);
    }


    public DataValue getDataValue() {
        Integer value = Integer.parseInt((String)getComboBox().getSelectedItem());
        OrdinalVO ordinalVO = Ordinals.getOrdinalVO(getIdTemplate(), getIdElement(), value);
        return new DvOrdinal(value, ordinalVO.getName(),ordinalVO.getTerminology(), ordinalVO.getCode());
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