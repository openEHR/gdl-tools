package se.cambio.openehr.view.panels;

import org.openehr.rm.datatypes.basic.DataValue;

import javax.swing.*;

public abstract class DVGenericPanel extends JPanel implements DVPanelInterface{

    /**
     *
     */
    private static final long serialVersionUID = 1L;
    protected String _idElement = null;
    protected String _idTemplate = null;
    protected boolean _allowsNull;
    protected boolean _requestFocus;

    public DVGenericPanel(String idElement, String idTemplate, boolean allowsNull, boolean requestFocus){
        _idElement = idElement;
        _idTemplate = idTemplate;
        _allowsNull = allowsNull;
        _requestFocus = requestFocus;
    }

    public String getIdElement() {
        return _idElement;
    }

    public void setIdElement(String idElement) {
        this._idElement = idElement;
    }

    public String getIdTemplate(){
        return _idTemplate;
    }

    public void setIdTemplate(String idTemplate){
        _idTemplate = idTemplate;
    }

    public boolean isAllowsNull() {
        return _allowsNull;
    }

    public void setAllowNull(boolean allowsNull) {
        _allowsNull = allowsNull;
    }

    public boolean isRequestFocus() {
        return _requestFocus;
    }

    public boolean isValidDV(){
        try{
            DataValue dv = getDataValue();
            if (dv!=null || _allowsNull){
                return true;
            }else{
                return false;
            }
        }catch(Exception e){
            return false;
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