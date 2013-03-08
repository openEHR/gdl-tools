package se.cambio.cds.model.facade.execution.vo;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

import se.cambio.cds.util.ExecutionLog;

public class RuleExecutionResult implements Serializable{

    /**
     * 
     */
    private static final long serialVersionUID = 30072012L;
    private Collection<ElementInstance> elementInstances = null;
    private List<ExecutionLog> log = null;
    private List<RuleReference> firedRules = null;
    
    public RuleExecutionResult(
	    Collection<ElementInstance> elementInstances,
	    List<ExecutionLog> log,
	    List<RuleReference> firedRules) {
	super();
	this.elementInstances = elementInstances;
	this.log = log;
	this.firedRules = firedRules;
    }
    
    public Collection<ElementInstance> getElementInstances() {
        return elementInstances;
    }
    public List<ExecutionLog> getLog() {
        return log;
    }
    public List<RuleReference> getFiredRules() {
        return firedRules;
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