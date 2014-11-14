package se.cambio.cds.model.app;

import se.cambio.cds.gdl.model.Language;
import se.cambio.cds.gdl.model.ResourceDescription;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * User: iago.corbal
 * Date: 2014-09-23
 * Time: 16:23
 */
public class CDSApp {

    private String cdsAppId;
    private Language language;
    private Map<String, CDSAppDefinition> cdsAppDefinitions;
    private ResourceDescription resourceDescription;
    private Collection<String> dsViewIds;

    public CDSApp(){
    }

    public CDSApp(String cdsAppId, Language language, Map<String, CDSAppDefinition> cdsAppDefinitions, ResourceDescription resourceDescription, Collection<String> dsViewIds) {
        this.cdsAppId = cdsAppId;
        this.language = language;
        this.cdsAppDefinitions = cdsAppDefinitions;
        this.resourceDescription = resourceDescription;
        this.dsViewIds = dsViewIds;
    }

    public String getCdsAppId() {
        return cdsAppId;
    }

    public void setCdsAppId(String cdsAppId) {
        this.cdsAppId = cdsAppId;
    }

    public Language getLanguage() {
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

    public Map<String, CDSAppDefinition> getCdsAppDefinitions() {
        if (cdsAppDefinitions == null) {
            cdsAppDefinitions = new HashMap<String, CDSAppDefinition>();
        }
        return cdsAppDefinitions;
    }

    public void setCdsAppDefinitions(Map<String, CDSAppDefinition> cdsAppDefinitions) {
        this.cdsAppDefinitions = cdsAppDefinitions;
    }

    public Collection<String> getDsViewIds() {
        if (dsViewIds == null) {
            dsViewIds = new ArrayList<String>();
        }
        return dsViewIds;
    }

    public void setDsViewIds(Collection<String> dsViewIds) {
        this.dsViewIds = dsViewIds;
    }
}
