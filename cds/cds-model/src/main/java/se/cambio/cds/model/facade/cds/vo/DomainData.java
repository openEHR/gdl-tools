package se.cambio.cds.model.facade.cds.vo;

import java.util.Collection;
import java.util.Map;

/**
 * User: iago.corbal
 * Date: 2013-11-25
 * Time: 09:08
 */
public class DomainData {

    private String domainId;
    private Map<String, Collection<Map<String, EIValue>>> ardvMap = null;

    public DomainData(String domainId, Map<String, Collection<Map<String, EIValue>>> ardvMap) {
        this.domainId = domainId;
        this.ardvMap = ardvMap;
    }

    public String getDomainId() {
        return domainId;
    }

    public void setDomainId(String domainId) {
        this.domainId = domainId;
    }

    public Map<String, Collection<Map<String, EIValue>>> getArdvMap() {
        return ardvMap;
    }

    public void setArdvMap(Map<String, Collection<Map<String, EIValue>>> ardvMap) {
        this.ardvMap = ardvMap;
    }
}
