package se.cambio.cm.model.util;

import se.cambio.cm.model.app.dto.CDSAppDTO;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.cm.model.kb.instance.dto.KBInstanceDTO;
import se.cambio.cm.model.orderset.dto.OrderSetDTO;
import se.cambio.cm.model.scenario.dto.ScenarioDTO;
import se.cambio.cm.model.study.dto.StudyDTO;
import se.cambio.cm.model.template.dto.TemplateDTO;
import se.cambio.cm.model.terminology.dto.TerminologyDTO;
import se.cambio.cm.model.view.dto.DSViewDTO;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Collection;
import java.util.Collections;

public enum CMType {

    TERMINOLOGY("terminologies", TerminologyDTO.class, Collections.singleton(CMTypeFormat.CSV_FORMAT.getFormat())),
    ARCHETYPE("archetypes", ArchetypeDTO.class, Collections.singleton(CMTypeFormat.ADL_FORMAT.getFormat())),
    TEMPLATE("templates", TemplateDTO.class, Collections.singleton(CMTypeFormat.OET_FORMAT.getFormat())),
    GUIDELINE("guidelines", GuideDTO.class, Collections.singleton(CMTypeFormat.GDL_FORMAT.getFormat())),
    VIEW("views", DSViewDTO.class, Collections.singleton(CMTypeFormat.DSV_FORMAT.getFormat())),
    STUDY("studies", StudyDTO.class, Collections.singleton(CMTypeFormat.STD_FORMAT.getFormat())),
    INSTANCE("instances", KBInstanceDTO.class, Collections.singleton(CMTypeFormat.KBI_FORMAT.getFormat())),
    ORDERSET("ordersets", OrderSetDTO.class, Collections.singleton(CMTypeFormat.OST_FORMAT.getFormat())),
    APP("apps", CDSAppDTO.class, Collections.singleton(CMTypeFormat.APP_FORMAT.getFormat())),
    SCENARIO("scenarios", ScenarioDTO.class, Collections.singleton(CMTypeFormat.SCN_FORMAT.getFormat()));

    private final String id;
    private Class<? extends CMElement> cmElementClass;
    private Collection<String> fileExtensions;

    CMType(String id, Class<? extends CMElement> cmElementClass, Collection<String> fileExtensions){
        this.id = id;
        this.cmElementClass = cmElementClass;
        this.fileExtensions = fileExtensions;
    }

    public Class<? extends CMElement> getCmElementClass() {
        return cmElementClass;
    }

    public Collection<String> getFileExtensions() {
        return fileExtensions;
    }

    public String getId() {
        return id;
    }

    public static CMType getCMTypeById(String id) throws InstanceNotFoundException {
        for (CMType cmType: CMType.values()){
            if (cmType.getId().equals(id)){
                return cmType;
            }
        }
        throw new InstanceNotFoundException(id, CMType.class.getName());
    }

    public static CMType getCMTypeByClass(Class<? extends CMElement> cmElementClass) throws InternalErrorException {
        for (CMType cmType: CMType.values()){
            if (cmType.getCmElementClass().equals(cmElementClass)){
                return cmType;
            }
        }
        throw new InternalErrorException(new InstanceNotFoundException(cmElementClass.getName(), CMElement.class.getName()));
    }

    public static Class<? extends CMElement> getCMElementClassById(String id) throws InstanceNotFoundException {
        CMType cmType = getCMTypeById(id);
        return cmType.getCmElementClass();
    }
}
