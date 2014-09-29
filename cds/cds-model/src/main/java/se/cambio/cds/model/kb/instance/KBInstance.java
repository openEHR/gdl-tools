package se.cambio.cds.model.kb.instance;

import org.openehr.rm.common.archetyped.Locatable;
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
    private ResourceDescription resourceDescription;
    private Map<String, KBInstanceDefinition> kbInstanceDefinitions;
    private Locatable locatable;

    public KBInstance() {
    }

    public KBInstance(
            String kbiId,
            ResourceDescription resourceDescription,
            Map<String, KBInstanceDefinition> kbInstanceDefinitions,
            Locatable locatable) {
        this.kbiId = kbiId;
        this.resourceDescription = resourceDescription;
        this.kbInstanceDefinitions = kbInstanceDefinitions;
        this.locatable = locatable;
    }

    public String getKbiId() {
        return kbiId;
    }

    public void setKbiId(String kbiId) {
        this.kbiId = kbiId;
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

    public Map<String, KBInstanceDefinition> getKbInstanceDefinitions() {
        return kbInstanceDefinitions;
    }

    public void setKbInstanceDefinitions(Map<String, KBInstanceDefinition> kbInstanceDefinitions) {
        if (kbInstanceDefinitions == null) {
            kbInstanceDefinitions = new HashMap<String, KBInstanceDefinition>();
        }
        this.kbInstanceDefinitions = kbInstanceDefinitions;
    }

    public Locatable getLocatable() {
        return locatable;
    }

    public void setLocatable(Locatable locatable) {
        this.locatable = locatable;
    }
}
