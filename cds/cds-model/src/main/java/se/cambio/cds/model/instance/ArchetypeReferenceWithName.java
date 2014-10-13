package se.cambio.cds.model.instance;

import java.util.Collection;


public class ArchetypeReferenceWithName extends ArchetypeReference{

    private static final long serialVersionUID = 1L;
    private String name = null;
    private String rmName = null;
    private boolean modified = false;

    public ArchetypeReferenceWithName(String idArchetype, String idTemplate, String name, String rmName) {
        super(null, idArchetype, idTemplate);
        this.name = name;
        this.rmName = rmName;
    }

    public void init(){
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getRMName() {
        return rmName;
    }

    public void setRMName(String rmName) {
        this.rmName = rmName;
    }

    public String getId(){
        return getIdArchetype();
    }


    public Collection<ElementInstance> getElementInstances() {
        return getElementInstancesMap().values();
    }

    public boolean isModified() {
        return modified;
    }

    public void setModified(boolean modified) {
        this.modified = modified;
    }
}