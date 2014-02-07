package se.cambio.openehr.model.template.dao;

import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.misc.OpenEHRConfigurationParametersManager;



/**
 * @author iago.corbal
 */
public class GenericTemplateFactory {

    private static String DAO_CLASS_TEMPLATE = "GenericTemplateDAO/Class";

    private GenericTemplateFactory() {
    }

    private static Class<?> getDAOClass() throws InternalErrorException {
        Class<?> theClass = null;
        try {
            String delegateClassName =
                    OpenEHRConfigurationParametersManager.getParameter(DAO_CLASS_TEMPLATE);
            theClass = Class.forName(delegateClassName);
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
        return theClass;
    }

    public static GenericTemplateDAO getDAO()
            throws InternalErrorException {
        try {
            return (GenericTemplateDAO)getDAOClass().newInstance();
        } catch (Exception e) {
            throw new InternalErrorException(e);
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