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

public enum CMType {

    ARCHETYPE("archetypes", ArchetypeDTO.class),
    TEMPLATE("templates", TemplateDTO.class),
    TERMINOLOGY("terminologies", TerminologyDTO.class),
    GUIDELINE("guidelines", GuideDTO.class),
    VIEW("views", DSViewDTO.class),
    STUDY("studies", StudyDTO.class),
    INSTANCE("instances", KBInstanceDTO.class),
    ORDERSET("ordersets", OrderSetDTO.class),
    SCENARIO("scenarios", ScenarioDTO.class),
    APP("apps", CDSAppDTO.class);

    private final String id;
    private Class<? extends CMElement> cmElementClass;

    CMType(String id, Class<? extends CMElement> cmElementClass){
        this.id = id;
        this.cmElementClass = cmElementClass;
    }

    public Class<? extends CMElement> getCmElementClass() {
        return cmElementClass;
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

    public static Class<? extends CMElement> getCMElementClassById(String id) throws InstanceNotFoundException {
        CMType cmType = getCMTypeById(id);
        return cmType.getCmElementClass();
    }
}
