package se.cambio.cds.model.facade.ehr.vo;

public class EHRTriggerVO {

    private String aqlStr = null;
    private boolean active = false;
    
    public EHRTriggerVO(String aqlStr, boolean active) {
	super();
	this.aqlStr = aqlStr;
	this.active = active;
    }
    
    public String getAqlStr() {
        return aqlStr;
    }
    public void setAqlStr(String aqlStr) {
        this.aqlStr = aqlStr;
    }
    public boolean isActive() {
        return active;
    }
    public void setActive(boolean active) {
        this.active = active;
    }
}
