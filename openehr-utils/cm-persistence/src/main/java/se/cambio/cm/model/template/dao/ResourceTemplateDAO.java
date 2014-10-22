package se.cambio.cm.model.template.dao;

import se.cambio.cm.model.cm.element.dao.ResourceGenericCMElementDAO;
import se.cambio.cm.model.template.dto.TemplateDTO;

import java.util.Collections;

public class ResourceTemplateDAO extends ResourceGenericCMElementDAO<TemplateDTO> {

    public ResourceTemplateDAO() {
        super(TemplateDTO.class, Collections.singleton("oet"));
    }
}
