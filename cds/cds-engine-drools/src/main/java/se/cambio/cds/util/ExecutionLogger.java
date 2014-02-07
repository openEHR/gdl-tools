package se.cambio.cds.util;

import org.apache.log4j.Logger;
import org.drools.spi.KnowledgeHelper;
import se.cambio.cds.controller.execution.DroolsExecutionManager;
import se.cambio.cds.model.facade.execution.vo.ExecutionLog;
import se.cambio.cds.model.instance.ElementInstance;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class ExecutionLogger {
    private List<ExecutionLog> _log = null;
    private List<String> _firedRules = null;
    private boolean cancelExecution = false;
    private long _startTime = 0;
    public ExecutionLogger(){
        _startTime = System.currentTimeMillis();
    }


    public void addLog(KnowledgeHelper drools, ElementInstance elementInstance){
        ExecutionLog executionLog =
                new ExecutionLog(
                        drools.getRule().getName(),
                        elementInstance.getArchetypeReference().getIdTemplate(),
                        elementInstance.getId(),
                        elementInstance.getDataValue()!=null?elementInstance.getDataValue().serialise():null);
        getLog().add(executionLog);

        //TODO This should not be done in the logger
        long executionTime = (System.currentTimeMillis()-_startTime);
        if (cancelExecution || executionTime> DroolsExecutionManager.getExecutionTimeOut()){
            Logger.getLogger(ExecutionLogger.class).warn("Execution canceled or timed out!");
            Map<String, Integer> countMap = new HashMap<String, Integer>();
            for (ExecutionLog logLine : getLog()){
                String firedRule = logLine.getFiredRule();
                int count = countMap.containsKey(firedRule) ? countMap.get(firedRule) : 0;
                countMap.put(firedRule, count + 1);
            }
            for (String firedRule:countMap.keySet()){
                Logger.getLogger(ExecutionLogger.class).info("Executed "+firedRule+" ("+countMap.get(firedRule)+")");
            }
            drools.halt();
        }
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

    public void cancelExecution(){
        cancelExecution = true;
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