package se.cambio.cds.model.view;

import se.cambio.cds.gdl.model.Language;
import se.cambio.cds.gdl.model.ResourceDescription;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * User: iago.corbal
 * Date: 2014-09-19
 * Time: 11:04
 */
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

    public DecisionSupportView(
            String dsViewId,
            Language language,
            Map<String, DecisionSupportViewDefinition> decisionSupportViewDefinitions,
            ResourceDescription resourceDescription,
            Collection<String> alertGuideIds,
            Collection<String> executionGuideIds,
            Map<String, TerminologyAlertBinding> alertBindings) {
        this.dsViewId = dsViewId;
        this.language = language;
        this.decisionSupportViewDefinitions = decisionSupportViewDefinitions;
        this.resourceDescription = resourceDescription;
        this.alertGuideIds = alertGuideIds;
        this.executionGuideIds = executionGuideIds;
        this.alertBindings = alertBindings;
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
