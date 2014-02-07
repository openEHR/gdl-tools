package se.cambio.cds.model.ehr;

import java.util.ArrayList;
import java.util.Collection;

public class EHRCase {

    private String idEHR = null;
    private Collection<EHRInstance> ehrInstances = null;
    private boolean enable = true;
    
    public EHRCase(String idEHR,
	    Collection<EHRInstance> ehrInstances) {
	super();
	this.idEHR = idEHR;
	this.ehrInstances = ehrInstances;
    }
    
    public EHRCase(String idEHR) {
	super();
	this.idEHR = idEHR;
	this.ehrInstances = new ArrayList<EHRInstance>();
    }
    public String getIdEHR() {
        return idEHR;
    }
    public void setIdEHR(String idEHR) {
        this.idEHR = idEHR;
    }
    public Collection<EHRInstance> getEHRInstances() {
        return ehrInstances;
    }
    public void setEHRInstances(
    	Collection<EHRInstance> ehrInstances) {
        this.ehrInstances = ehrInstances;
    }
    public boolean isEnable() {
        return enable;
    }

    public void setEnable(boolean enable) {
        this.enable = enable;
    }
}
