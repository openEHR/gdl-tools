package se.cambio.cds.model.facade.execution.delegate;

import se.cambio.cds.util.misc.CDSConfigurationParametersManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

/**
 * @author iago.corbal
 *
 */
public class RuleExecutionFacadeDelegateFactory {

    private static String DELEGATE_CLASS_EXECUTION = "RuleExecutionFacadeDelegate/Class";

	private RuleExecutionFacadeDelegateFactory() {
	}

    private static Class<?> getDelegateClass() throws InternalErrorException {
        Class<?> theClass = null;
        try {
            String delegateClassName =
                    CDSConfigurationParametersManager.getParameter(DELEGATE_CLASS_EXECUTION);
            theClass = Class.forName(delegateClassName);
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
        return theClass;
    }

    public static RuleExecutionFacadeDelegate getDelegate()
            throws InternalErrorException {
        try {
            return (RuleExecutionFacadeDelegate)getDelegateClass().newInstance();
        } catch (InstantiationException e) {
            throw new InternalErrorException(e);
        } catch (IllegalAccessException e) {
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