package se.cambio.cds.util;

import com.rits.cloning.Cloner;
import org.apache.log4j.Logger;
import org.openehr.am.archetype.Archetype;
import org.openehr.rm.common.archetyped.Locatable;
import org.openehr.rm.composition.content.entry.Activity;
import org.openehr.rm.datastructure.itemstructure.representation.Element;
import org.openehr.rm.util.GenerationStrategy;
import org.openehr.rm.util.SkeletonGenerator;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ContainerInstance;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.cm.model.template.dto.TemplateDTO;
import se.cambio.openehr.controller.session.data.ArchetypeElements;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.OpenEHRRMUtil;

import java.util.*;

public class LocatableUtil {
    public static Locatable createLocatable(ArchetypeReference archetypeReference) throws Exception{
        Map<String, Object> extraValues = new HashMap<String, Object>();
        Archetype archetype = null;
        if (archetypeReference.getIdTemplate()!=null){
            TemplateDTO templateDTO = getTemplates().getCMElement(archetypeReference.getIdTemplate());
            archetype = (Archetype) IOUtils.getObject(templateDTO.getAom());
        }else{
            Set<String> singleton = Collections.singleton(archetypeReference.getIdArchetype());
            archetype = getArchetypes().getArchetypeAOMsByIds(singleton).iterator().next();
        }
        Locatable locatable =
                (Locatable)SkeletonGenerator.getInstance().create(archetype, archetypeReference.getIdTemplate(), getArchetypes().getArchetypeMap(), extraValues, GenerationStrategy.MAXIMUM_EMPTY);
        HashMap<ContainerInstance, Locatable> containersMap = new HashMap<ContainerInstance, Locatable>();
        Collection<ArchetypeElementVO> archetypeElements =
                getArchetypeElements().getArchetypeElementsVO(archetypeReference.getIdArchetype(), archetypeReference.getIdTemplate());
        //TODO Hack for medication activity
        if (locatable.itemAtPath("/activities[at0001]")!=null){
            Object obj = locatable.itemAtPath("/activities[at0001]");
            if (obj instanceof Activity){
                Activity a = ((Activity)obj);
                a.setActionArchetypeId("openEHR-EHR-ACTION.medication.v1");
            }
        }
        for (ArchetypeElementVO archetypeElement : archetypeElements) {
            ElementInstance elementInstance = archetypeReference.getElementInstancesMap().get(archetypeElement.getId());
            //TODO Save all the instances!!
            //for (ElementInstance elementInstance : elementInstances) {
            Locatable locatableAux = locatable;
            String pathAux = archetypeElement.getPath();
            if (elementInstance!=null && elementInstance.getContainerInstance()!=null){
                String containerId = elementInstance.getContainerInstance().getId();
                try{
                    locatableAux = getContainerLocatable(elementInstance.getContainerInstance(), locatable, containersMap);
                }catch(IllegalArgumentException e){
                    Logger.getLogger(LocatableUtil.class).warn("Container locatable could not be obtained for '"+containerId+"'. Will be skipped"); //TODO
                    continue;
                }
                pathAux = elementInstance.getId().substring(containerId.length());
                if (locatableAux==null){
                    Logger.getLogger(LocatableUtil.class).warn("Locatable for '"+containerId+"' is empty. Will be skipped."); //TODO
                    continue;
                }
            }

            if (elementInstance!=null && elementInstance.getDataValue()!=null){
                //TODO Fix!
                if (!OpenEHRRMUtil.isRMPath(pathAux)){
                    Object obj = locatableAux.itemAtPath(pathAux);
                    if (obj instanceof Element){
                        ((Element)obj).setValue(elementInstance.getDataValue());
                        ((Element)obj).setNullFlavour(null);
                    }
                }
            }else{
                Object obj = locatableAux.itemAtPath(pathAux);
                //ArchetypeConstraint ac = archetype.node(pathAux);
                if (obj instanceof Element){
                    Element e = ((Element)obj);
                    e.setNullFlavour(OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
                    e.setValue(null);
                }
            }
            //}
        }
        return locatable;
    }

    public static Locatable getContainerLocatable(ContainerInstance containerInstance, Locatable locatable, HashMap<ContainerInstance, Locatable> containersMap){
        Locatable containerLocatable = containersMap.get(containerInstance);
        if (containerLocatable==null){
            ContainerInstance parentContainerInstance = containerInstance.getParentContainerInstance();
            if (parentContainerInstance!=null){
                String containerPath = containerInstance.getId().replace(parentContainerInstance.getId(), "");
                Locatable parentContainerLocatable = getContainerLocatable(parentContainerInstance, locatable, containersMap);
                containerLocatable = (Locatable)parentContainerLocatable.itemAtPath(containerPath);
                if (existsLocatableInMap(containerLocatable, containersMap)){
                    if (containerPath.lastIndexOf("[")>containerPath.lastIndexOf(Locatable.PATH_SEPARATOR)){
                        containerLocatable = new Cloner().deepClone((Locatable)parentContainerLocatable.itemAtPath(containerPath));
                        String containerPathAux = containerPath.substring(0, containerPath.lastIndexOf("["));
                        parentContainerLocatable.addChild(containerPathAux, containerLocatable);
                    }else{
                        //No container instance should be created for this branches
                        ExceptionHandler.handle(new Exception("Warning! Branch '"+containerPath+"' not handled"));
                    }
                }
            }else{
                //TODO Works?
                containerLocatable = locatable;
            }
            containersMap.put(containerInstance, containerLocatable);
        }
        return containerLocatable;
    }

    private static boolean existsLocatableInMap(Locatable locatable, HashMap<ContainerInstance, Locatable> containersMap){
        boolean found = false;
        Iterator<Locatable> i = containersMap.values().iterator();
        while(i.hasNext()){
            if (i.next()==locatable){
                found = true;
                break;
            }
        }
        return found;
    }

    public static ArchetypeElements getArchetypeElements(){
        return ArchetypeManager.getInstance().getArchetypeElements();
    }

    public static Templates getTemplates() {
        return ArchetypeManager.getInstance().getTemplates();
    }

    public static Archetypes getArchetypes() {
        return ArchetypeManager.getInstance().getArchetypes();
    }
}
