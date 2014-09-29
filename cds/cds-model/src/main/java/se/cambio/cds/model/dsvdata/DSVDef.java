package se.cambio.cds.model.dsvdata;

import java.util.HashMap;
import java.util.Map;

public class DSVDef {

    private String dsvId;
    private Map<String, DSVGuideDef> dsvGuideDefMap;

    public DSVDef(String dsvId) {
        this.dsvId = dsvId;
    }

    public String getDsvId() {
        return dsvId;
    }

    public void setDsvId(String dsvId) {
        this.dsvId = dsvId;
    }

    public void addDSVGuideDef(DSVGuideDef dsvGuideDef){
        getDsvGuideDefMap().put(dsvGuideDef.getGuideId(), dsvGuideDef);
    }

    public Map<String, DSVGuideDef> getDsvGuideDefMap(){
        if (dsvGuideDefMap == null) {
            dsvGuideDefMap = new HashMap<String, DSVGuideDef>();
        }
        return dsvGuideDefMap;
    }

    public void setDsvGuideDefMap(Map<String, DSVGuideDef> dsvGuideDefMap) {
        this.dsvGuideDefMap = dsvGuideDefMap;
    }
}
