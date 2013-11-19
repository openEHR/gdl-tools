package se.cambio.cds.model.instance;

import java.util.Collection;


public class ArchetypeReferenceWithName extends ArchetypeReference{

    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String name = null;
    private String rmName = null;
    private boolean modified = false;
    //private Map<String, Collection<ElementInstance>> elementInstancesVOsByPath = null;
    //private Map<ClusterVO, ContainerInstance> containerInstancesMap = null;

    public ArchetypeReferenceWithName(String idArchetype, String idTemplate, String name, String rmName, boolean init) {
	super(null, idArchetype, idTemplate);
	this.name = name;
	this.rmName = rmName;
	//this.elementInstancesVOsByPath = new HashMap<String, Collection<ElementInstance>>();
	//if (init){
	//    init();
	//}
    }

    public void init(){
	/*
	if (getIdArchetype()!=null){   
	    for (ArchetypeElementVO archetypeElementVO : ArchetypeElements.getArchetypeElementsVO(getIdArchetype(), getIdTemplate())) {
		String path = archetypeElementVO.getPath();
		getElementInstancesAtPath(path);
	    }
	}
	if (getIdTemplate()!=null){
	    Collection<ElementInstance> elementInstances = elementInstancesVOsByPath.get("/archetype_details/template_id");
	    //TemplateID is always included
	    ElementInstance elementInstance = elementInstances.iterator().next();
	    elementInstance.setDataValue(new DvText(getIdTemplate()));
	    elementInstance.setNullFlavour(null);
	}*/
    }
    /*
    private ContainerInstance getContainerInstance(String idTemplate, String id){
	ClusterVO clusterVO = Clusters.getClusterVO(idTemplate, id);
	if (clusterVO!=null){
	    ContainerInstance containerInstance = getContainerInstancesMap().get(clusterVO);
	    if(containerInstance==null){
		ContainerInstance parentContainerInstance = 
			getContainerInstance(clusterVO.getIdTemplate(), clusterVO.getIdParent());
		containerInstance = new ContainerInstance(clusterVO.getId(), parentContainerInstance);
		getContainerInstancesMap().put(clusterVO, containerInstance);
	    }
	    return containerInstance;
	}else {
	    return null;
	}
    }

    
    private Map<ClusterVO, ContainerInstance> getContainerInstancesMap(){
	if (containerInstancesMap==null){
	    containerInstancesMap = new HashMap<ClusterVO, ContainerInstance>();
	}
	return containerInstancesMap;
    }*/

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

    /*
    private Map<String, Collection<ElementInstance>> getElementInstancesByPath() {
	return elementInstancesVOsByPath;
    }

    public Collection<String> getElementInstancePaths() {
	return elementInstancesVOsByPath.keySet();
    }
    */

    public Collection<ElementInstance> getElementInstances() {
	/*
	ArrayList<ElementInstance> totalElementInstances = new ArrayList<ElementInstance>();
	for (Collection<ElementInstance> elementInstances : elementInstancesVOsByPath.values()) {
	    totalElementInstances.addAll(elementInstances);
	}
	return totalElementInstances;
	*/
	return getElementInstancesMap().values();
    }

    /*
    public Collection<ElementInstance> getElementInstancesAtPath(String path){
	Collection<ElementInstance> elements = getElementInstancesByPath().get(path);
	if (elements==null){
	    String id = getIdArchetype()+path;
	    ArchetypeElementVO archetypeElement = ArchetypeElements.getArchetypeElement(getIdTemplate(), id);
	    if (archetypeElement!=null){
		elements = new ArrayList<ElementInstance>();
		ElementInstance elementInstance = new ElementInstance(
			id,
			null,
			this,
			getContainerInstance(getIdTemplate(), archetypeElement.getIdParent()),
			InstanceEditorController.NULL_FLAVOUR_VALUE);
		ArrayList<ElementInstance> elementInstances = new ArrayList<ElementInstance>();
		elementInstances.add(elementInstance);
		elements.add(elementInstance);
		getElementInstancesByPath().put(path, elements);
	    }
	}
	return elements;
    }

    public ElementInstance getSingleElementInstance(String path){
	Collection<ElementInstance> elements = getElementInstancesAtPath(path);
	if (elements!=null){
	    return elements.iterator().next();
	}else{
	    return null;
	}
    }

    public void addElementInstance(ElementInstance elementInstance){
	String id = elementInstance.getId();
	String path = id.substring(id.indexOf(Locatable.PATH_SEPARATOR));
	Collection<ElementInstance> elementInstances = elementInstancesVOsByPath.get(path); 
	if (elementInstances==null){
	    elementInstances = new ArrayList<ElementInstance>();
	    elementInstancesVOsByPath.put(path, elementInstances);
	}
	elementInstances.add(elementInstance);
    }
    public void removeElementInstance(ElementInstance elementInstance){
	String id = elementInstance.getId();
	String path = id.substring(id.indexOf(Locatable.PATH_SEPARATOR));
	elementInstancesVOsByPath.get(path).remove(elementInstance);
    }*/


    public boolean isModified() {
        return modified;
    }

    public void setModified(boolean modified) {
        this.modified = modified;
    }
}