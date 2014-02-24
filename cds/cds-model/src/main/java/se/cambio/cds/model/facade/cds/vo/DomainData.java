package se.cambio.cds.model.facade.cds.vo;

import java.util.LinkedHashMap;
import java.util.List;

/**
 * User: iago.corbal
 * Date: 2013-11-25
 * Time: 09:08
 */
public class DomainData {

    private String domainId;
    private LinkedHashMap<String, List<LinkedHashMap<String, EIValue>>> ardvMap = null;

    public DomainData(String domainId, LinkedHashMap<String, List<LinkedHashMap<String, EIValue>>> ardvMap) {
        this.domainId = domainId;
        this.ardvMap = ardvMap;
    }

    public String getDomainId() {
        return domainId;
    }

    public void setDomainId(String domainId) {
        this.domainId = domainId;
    }

    public LinkedHashMap<String, List<LinkedHashMap<String, EIValue>>> getArdvMap() {
        return ardvMap;
    }

    public void setArdvMap(LinkedHashMap<String, List<LinkedHashMap<String, EIValue>>> ardvMap) {
        this.ardvMap = ardvMap;
    }
}
