package se.cambio.cds.model.view;

import se.cambio.cds.gdl.model.Language;
import se.cambio.cds.gdl.model.ResourceDescription;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public class DecisionSupportView {
    private String dsViewId;
    private Language language;
    private Map<String, DecisionSupportViewDefinition> decisionSupportViewDefinitions;
    private ResourceDescription resourceDescription;
    private Collection<String> alertGuideIds;
    private Collection<String> executionGuideIds;
    private Map<String, TerminologyAlertBinding> alertBindings;

    public DecisionSupportView() {
    }

    public String getDsViewId() {
        return dsViewId;
    }

    public void setDsViewId(String dsViewId) {
        this.dsViewId = dsViewId;
    }

    public Language getLanguage() {
        if (language == null) {
            language = new Language();
        }
        return language;
    }

    public void setLanguage(Language language) {
        this.language = language;
    }

    public ResourceDescription getResourceDescription() {
        if (resourceDescription == null) {
            resourceDescription = new ResourceDescription();
        }
        return resourceDescription;
    }

    public void setResourceDescription(ResourceDescription resourceDescription) {
        this.resourceDescription = resourceDescription;
    }

    public Collection<String> getAlertGuideIds() {
        if (alertGuideIds == null) {
            alertGuideIds = new ArrayList<String>();
        }
        return alertGuideIds;
    }

    public void setAlertGuideIds(Collection<String> alertGuideIds) {
        this.alertGuideIds = alertGuideIds;
    }

    public Collection<String> getExecutionGuideIds() {
        if (executionGuideIds == null) {
            executionGuideIds = new ArrayList<String>();
        }
        return executionGuideIds;
    }

    public void setExecutionGuideIds(Collection<String> executionGuideIds) {
        this.executionGuideIds = executionGuideIds;
    }

    public Map<String, DecisionSupportViewDefinition> getDecisionSupportViewDefinitions() {
        if (decisionSupportViewDefinitions == null) {
            decisionSupportViewDefinitions = new HashMap<String, DecisionSupportViewDefinition>();
        }
        return decisionSupportViewDefinitions;
    }

    public void setDecisionSupportViewDefinitions(Map<String, DecisionSupportViewDefinition> decisionSupportViewDefinitions) {
        this.decisionSupportViewDefinitions = decisionSupportViewDefinitions;
    }

    public Map<String, TerminologyAlertBinding> getAlertBindings() {
        if (alertBindings == null) {
            alertBindings = new HashMap<String, TerminologyAlertBinding>();
        }
        return alertBindings;
    }

    public void setAlertBindings(Map<String, TerminologyAlertBinding> alertBindings) {
        this.alertBindings = alertBindings;
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