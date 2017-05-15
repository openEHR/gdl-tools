package se.cambio.cds.util;

import org.drools.core.spi.KnowledgeHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cds.controller.execution.DroolsExecutionManager;
import se.cambio.cds.model.facade.execution.vo.ExecutionLog;
import se.cambio.cds.model.instance.ElementInstance;

import java.util.*;


public class ExecutionLogger {
    private List<ExecutionLog> log = null;
    private List<String> firedRules = null;
    private boolean executionCanceled = false;
    private boolean executionTimedOut = false;
    private boolean halt = false;
    private Set<ElementInstance> elementInstancesSet = null;
    private long startTime = 0;
    private static Logger logger = LoggerFactory.getLogger(ExecutionLogger.class);


    public ExecutionLogger() {
        startTime = System.currentTimeMillis();
    }


    public void addLog(KnowledgeHelper drools, ElementInstance elementInstance) {
        getElementInstancesSet().add(elementInstance);
        final ExecutionLog executionLog =
                new ExecutionLog(
                        drools.getRule().getName(),
                        elementInstance.getArchetypeReference().getIdTemplate(),
                        elementInstance.getId(),
                        elementInstance.getDataValue() != null ? elementInstance.getDataValue().serialise() : null);
        getLog().add(executionLog);
        //TODO This should not be done in the logger
        checkForExecutionTimeout(drools);
    }

    private void checkForExecutionTimeout(KnowledgeHelper drools) {
        if (!halt) {
            long executionTime = (System.currentTimeMillis() - startTime);
            if (!executionTimedOut) {
                executionTimedOut = executionTime > DroolsExecutionManager.DEFAULT_TIMEOUT;
            }
            if (executionCanceled || executionTimedOut) {
                logger.warn("Execution canceled or timed out! (executionTime= " + executionTime + " ms)");
                Map<String, Integer> countMap = new HashMap<String, Integer>();
                for (ExecutionLog logLine : getLog()) {
                    String firedRule = logLine.getFiredRule();
                    int count = countMap.containsKey(firedRule) ? countMap.get(firedRule) : 0;
                    countMap.put(firedRule, count + 1);
                }
                for (Map.Entry<String, Integer> entry : countMap.entrySet()) {
                    logger.info("Executed " + entry.getKey() + " (" + entry.getValue() + ")");
                }
                drools.halt();
                halt = true;
            }
        }
    }

    public List<ExecutionLog> getLog() {
        if (log == null) {
            log = new ArrayList<>();
        }
        return log;
    }

    public void setFiredRules(List<String> firedRules) {
        this.firedRules = firedRules;
    }

    public List<String> getFiredRules() {
        return firedRules;
    }

    public void cancelExecution() {
        executionCanceled = true;
    }

    public boolean executionCanceled() {
        return executionCanceled;
    }

    public boolean executionTimedOut() {
        return executionTimedOut;
    }

    public Set<ElementInstance> getElementInstancesSet() {
        if (elementInstancesSet == null) {
            elementInstancesSet = new HashSet<>();
        }
        return elementInstancesSet;
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