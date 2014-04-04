package se.cambio.cds.overview.model;

import java.util.HashMap;
import java.util.Map;

/**
 * User: Iago.Corbal
 * Date: 2014-02-19
 * Time: 11:14
 */
public class DSVDef {

    private String dsvId;
    private Map<String, DSVGuideDef> dsvGuideDefMap = new HashMap<String, DSVGuideDef>();

    public DSVDef(String dsvId) {
        this.dsvId = dsvId;
    }

    public String getDsvId() {
        return dsvId;
    }

    public void setDsvId(String dsvId) {
        this.dsvId = dsvId;
    }

    public Map<String, DSVGuideDef> getDsvGuideDefMap() {
        return dsvGuideDefMap;
    }

    public void setDsvGuideDefMap(Map<String, DSVGuideDef> dsvGuideDefMap) {
        this.dsvGuideDefMap = dsvGuideDefMap;
    }
}
