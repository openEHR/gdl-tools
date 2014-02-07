package se.cambio.cds.util;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

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
import se.cambio.openehr.controller.session.data.ArchetypeElements;
import se.cambio.openehr.controller.session.data.ArchetypeSlots;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.model.archetype.vo.ArchetypeSlotVO;
import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.OpenEHRConstUI;

import com.rits.cloning.Cloner;

public class LocatableUtil {
    public static Locatable createLocatable(ArchetypeReference archetypeReference) throws Exception{
        Map<String, Object> extraValues = new HashMap<String, Object>();
        if (archetypeReference.getIdTemplate()==null){
            Collection<ArchetypeSlotVO> archetypeSlots =
                    ArchetypeSlots.getArchetypeSlots(archetypeReference.getIdArchetype());
            for (ArchetypeSlotVO archetypeSlotVO : archetypeSlots) {
                Iterator<String> i = archetypeSlotVO.getIncludes().iterator();
                Archetype archetype = null;
                //Try to guess the archetype
                while(i.hasNext() && archetype==null){
                    String includeStr = i.next();
                    includeStr = includeStr.replaceAll("\\\\","");
                    String[] splitStr = includeStr.split("\\|");
                    int k = 0;
                    while (splitStr.length>k && archetype==null){
                        String idArchetype = splitStr[k++].trim();
                        archetype = Archetypes.getArchetypeAOM(idArchetype);
                    }
                }
                if (archetype!=null){
                    Locatable locatable =
                            (Locatable)SkeletonGenerator.getInstance().create(archetype, null, Archetypes.getArchetypeMap(), extraValues, GenerationStrategy.MAXIMUM_EMPTY);
                    extraValues.put(archetypeSlotVO.getPath(), locatable);
                }
            }
        }
        Archetype archetype = null;
        if (archetypeReference.getIdTemplate()!=null){
            TemplateDTO templateDTO = Templates.getTemplateDTO(archetypeReference.getIdTemplate());
            archetype = (Archetype) IOUtils.getObject(templateDTO.getAom());
        }else{
            archetype = Archetypes.getArchetypeAOM(archetypeReference.getIdArchetype());
        }
        Locatable locatable =
                (Locatable)SkeletonGenerator.getInstance().create(archetype, archetypeReference.getIdTemplate(), Archetypes.getArchetypeMap(), extraValues, GenerationStrategy.MAXIMUM_EMPTY);
        HashMap<ContainerInstance, Locatable> containersMap = new HashMap<ContainerInstance, Locatable>();
        Collection<ArchetypeElementVO> archetypeElements =
                ArchetypeElements.getArchetypeElementsVO(archetypeReference.getIdArchetype(), archetypeReference.getIdTemplate());
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
                if (!pathAux.contains("/archetype_details/template_id") && !pathAux.contains("/event/time")){
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
	    /*


 	    if (existsContainerIdInMap(containerInstance.getId(), containersMap)){
 		containerLocatable = new Cloner().deepClone((Locatable)locatable.itemAtPath(containerPath));
 		if (containerPath.lastIndexOf("[")>containerPath.lastIndexOf(Locatable.PATH_SEPARATOR)){
 		    String containerPathAux = containerPath.substring(0, containerPath.lastIndexOf("["));
 		    if (locatable.itemAtPath(containerPathAux) instanceof List){
 			containerPath = containerPathAux;
 		    }
 		}
 		locatable.addChild(containerPath, containerLocatable);
 	    }else{
 		containerLocatable = 
 	    }
	     */
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
}
