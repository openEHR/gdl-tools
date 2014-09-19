package se.cambio.cds.model.dsvdata;

import java.util.HashMap;
import java.util.Map;

/**
 * User: Iago.Corbal
 * Date: 2014-02-19
 * Time: 11:17
 */
public class DSVGuideDef {

    private String guideId;
    private Map<String, DSVGTCodeDef> dsvgtCodeDefMap = new HashMap<String, DSVGTCodeDef>();

    public DSVGuideDef(String guideId) {
        this.guideId = guideId;
    }

    public String getGuideId() {
        return guideId;
    }

    public void setGuideId(String guideId) {
        this.guideId = guideId;
    }

    public Map<String, DSVGTCodeDef> getDsvgtCodeDefMap() {
        return dsvgtCodeDefMap;
    }

    public void setDsvgtCodeDefMap(Map<String, DSVGTCodeDef> dsvgtCodeDefMap) {
        this.dsvgtCodeDefMap = dsvgtCodeDefMap;
    }
}
