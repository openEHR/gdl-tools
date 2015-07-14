package se.cambio.cds.model.facade.execution.vo;

import se.cambio.cds.model.instance.ArchetypeReference;

import java.io.Serializable;
import java.util.Collection;
import java.util.Date;
import java.util.List;

public class RuleExecutionResult implements Serializable {

    private static final long serialVersionUID = 30072012L;
    private String ehrId = null;
    private Date date;
    private Collection<ArchetypeReference> ars = null;
    private List<ExecutionLog> log = null;
    private List<RuleReference> firedRules = null;
    private boolean timedOut = false;

    public RuleExecutionResult(
            String ehrId,
            Date date,
            Collection<ArchetypeReference> ars,
            List<ExecutionLog> log,
            List<RuleReference> firedRules) {
        super();
        this.ehrId = ehrId;
        this.date = date;
        this.ars = ars;
        this.log = log;
        this.firedRules = firedRules;
    }

    public String getEhrId() {
        return ehrId;
    }

    public void setEhrId(String ehrId) {
        this.ehrId = ehrId;
    }

    public Date getDate() {
        return date;
    }

    public void setDate(Date date) {
        this.date = date;
    }

    public Collection<ArchetypeReference> getArchetypeReferences() {
        return ars;
    }

    public List<ExecutionLog> getLog() {
        return log;
    }

    public List<RuleReference> getFiredRules() {
        return firedRules;
    }

    public boolean isTimedOut() {
        return timedOut;
    }

    public void setTimedOut(boolean timedOut) {
        this.timedOut = timedOut;
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