package se.cambio.cds.model.scenario;

import se.cambio.cds.gdl.model.Language;
import se.cambio.cds.gdl.model.ResourceDescription;

import java.util.*;

/**
 * User: iago.corbal
 * Date: 2014-09-23
 * Time: 16:30
 */
public class Scenario {
    private String scenarioId;
    private Language language;
    private Map<String, ScenarioDefinition> scenarioDefinitions;
    private ResourceDescription resourceDescription;
    private Collection<String> guideIds;
    private Date executionDate;

    public Scenario() {
    }

    public Scenario(String scenarioId, Language language, Map<String, ScenarioDefinition> scenarioDefinitions, ResourceDescription resourceDescription, Collection<String> guideIds, Date executionDate) {
        this.scenarioId = scenarioId;
        this.language = language;
        this.scenarioDefinitions = scenarioDefinitions;
        this.resourceDescription = resourceDescription;
        this.guideIds = guideIds;
        this.executionDate = executionDate;
    }

    public String getScenarioId() {
        return scenarioId;
    }

    public void setScenarioId(String scenarioId) {
        this.scenarioId = scenarioId;
    }

    public Language getLanguage() {
        return language;
    }

    public void setLanguage(Language language) {
        this.language = language;
    }

    public Map<String, ScenarioDefinition> getScenarioDefinitions() {
        if (scenarioDefinitions == null) {
            scenarioDefinitions = new HashMap<String, ScenarioDefinition>();
        }
        return scenarioDefinitions;
    }

    public void setScenarioDefinitions(Map<String, ScenarioDefinition> scenarioDefinitions) {
        this.scenarioDefinitions = scenarioDefinitions;
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

    public Collection<String> getGuideIds() {
        if (guideIds == null) {
            guideIds = new ArrayList<String>();
        }
        return guideIds;
    }

    public void setGuideIds(Collection<String> guideIds) {
        this.guideIds = guideIds;
    }

    public Date getExecutionDate() {
        return executionDate;
    }

    public void setExecutionDate(Date executionDate) {
        this.executionDate = executionDate;
    }
}

