package se.cambio.openehr.model.template.dao;

import se.cambio.openehr.model.cm.element.dao.ResourceGenericCMElementDAO;
import se.cambio.openehr.model.template.dto.TemplateDTO;

import java.util.Collections;

public class ResourceTemplateDAO extends ResourceGenericCMElementDAO<TemplateDTO> {

    public ResourceTemplateDAO() {
        super(TemplateDTO.class, Collections.singleton("oet"));
    }
}
