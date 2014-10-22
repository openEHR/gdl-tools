package se.cambio.cm.model.util;

import se.cambio.cm.model.app.dto.CDSAppDTO;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.cm.model.kb.instance.dto.KBInstanceDTO;
import se.cambio.cm.model.orderset.dto.OrderSetDTO;
import se.cambio.cm.model.template.dto.TemplateDTO;
import se.cambio.cm.model.terminology.dto.TerminologyDTO;
import se.cambio.cm.model.view.dto.DSViewDTO;

public enum CMType {
    ARCHETYPE(ArchetypeDTO.class),
    TEMPLATE(TemplateDTO.class),
    TERMINOLOGY(TerminologyDTO.class),
    GUIDELINE(GuideDTO.class),
    VIEW(DSViewDTO.class),
    INSTANCE(KBInstanceDTO.class),
    ORDERSET(OrderSetDTO.class),
    APP(CDSAppDTO.class);

    private Class<? extends CMElement> cmElementClass;

    CMType(Class<? extends CMElement> cmElementClass){
        this.cmElementClass = cmElementClass;
    }

    public Class<? extends CMElement> getCmElementClass() {
        return cmElementClass;
    }

    public void setCmElementClass(Class<? extends CMElement> cmElementClass) {
        this.cmElementClass = cmElementClass;
    }
}
