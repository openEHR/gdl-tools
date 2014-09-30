package se.cambio.cds.model.orderset;

import se.cambio.cds.gdl.model.Language;
import se.cambio.cds.gdl.model.ResourceDescription;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public class OrderSet {
    private String orderSetId;
    private Language language;
    private ResourceDescription resourceDescription;
    private Map<String, OrderSetDefinition> orderSetDefinitions;
    private Collection<String> guideIds;

    public OrderSet(String orderSetId) {
        this.orderSetDefinitions = orderSetDefinitions;
    }

    public String getOrderSetId() {
        return orderSetId;
    }

    public void setOrderSetId(String orderSetId) {
        this.orderSetId = orderSetId;
    }

    public Language getLanguage() {
        return language;
    }

    public void setLanguage(Language language) {
        this.language = language;
    }

    public ResourceDescription getResourceDescription() {
        return resourceDescription;
    }

    public void setResourceDescription(ResourceDescription resourceDescription) {
        if (resourceDescription == null) {
            resourceDescription = new ResourceDescription();
        }
        this.resourceDescription = resourceDescription;
    }

    public Map<String, OrderSetDefinition> getOrderSetDefinitions() {
        if (orderSetDefinitions == null) {
            orderSetDefinitions = new HashMap<String, OrderSetDefinition>();
        }
        return orderSetDefinitions;
    }

    public void setOrderSetDefinitions(Map<String, OrderSetDefinition> orderSetDefinitions) {
        this.orderSetDefinitions = orderSetDefinitions;
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
}
