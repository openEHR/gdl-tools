package se.cambio.cds.model.ehr;

import se.cambio.cds.model.instance.ArchetypeReferenceWithName;

public class EHRInstance extends ArchetypeReferenceWithName{

    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private boolean enable = true;

    public EHRInstance(String idArchetype, String idTemplate, String name, String rmName, boolean init) {
	super(idArchetype, idTemplate, name, rmName, init);
    }
    public boolean isEnable() {
	return enable;
    }

    public void setEnable(boolean enable) {
	this.enable = enable;
    }

    public void init(){
	super.init();
    }

}
