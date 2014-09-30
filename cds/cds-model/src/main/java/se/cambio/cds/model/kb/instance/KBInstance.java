package se.cambio.cds.model.kb.instance;

import org.openehr.rm.common.archetyped.Locatable;
import se.cambio.cds.gdl.model.Language;
import se.cambio.cds.gdl.model.ResourceDescription;

import java.util.HashMap;
import java.util.Map;

/**
 * User: iago.corbal
 * Date: 2014-09-04
 * Time: 12:00
 */
public class KBInstance {
    private String kbiId;
    private Language language;
    private ResourceDescription resourceDescription;
    private Map<String, KBInstanceDefinition> kbInstanceDefinitions;
    private Locatable locatable;

    public KBInstance(String kbiId) {
        this.kbiId = kbiId;
    }

    public String getKbiId() {
        return kbiId;
    }

    public void setKbiId(String kbiId) {
        this.kbiId = kbiId;
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

    public Map<String, KBInstanceDefinition> getKbInstanceDefinitions() {
        if (kbInstanceDefinitions == null) {
            kbInstanceDefinitions = new HashMap<String, KBInstanceDefinition>();
        }
        return kbInstanceDefinitions;
    }

    public void setKbInstanceDefinitions(Map<String, KBInstanceDefinition> kbInstanceDefinitions) {
        this.kbInstanceDefinitions = kbInstanceDefinitions;
    }

    public Locatable getLocatable() {
        return locatable;
    }

    public void setLocatable(Locatable locatable) {
        this.locatable = locatable;
    }

}
