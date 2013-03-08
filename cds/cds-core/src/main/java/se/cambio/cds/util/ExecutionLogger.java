package se.cambio.cds.util;

import java.util.ArrayList;
import java.util.List;

import org.drools.spi.KnowledgeHelper;

import se.cambio.cds.model.facade.execution.vo.ElementInstance;


public class ExecutionLogger {
    private List<ExecutionLog> _log = null;
    private List<String> _firedRules = null;

    public void addLog(KnowledgeHelper drools, ElementInstance elementInstance){
	ExecutionLog executionLog = 
		new ExecutionLog(
			drools.getRule().getName(),
			elementInstance.getArchetypeReference().getIdTemplate(),
			elementInstance.getId(),
			elementInstance.getDataValue()!=null?elementInstance.getDataValue().serialise():null);
	getLog().add(executionLog);
    }

    public List<ExecutionLog> getLog(){
	if (_log==null){
	    _log = new ArrayList<ExecutionLog>();
	}
	return _log;
    }

    public void setFiredRules(List<String> firedRules){
	_firedRules = firedRules;
    }
    
    public List<String> getFiredRules(){
	return _firedRules;
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