package se.cambio.cm.model.template.dao;

import se.cambio.cm.model.generic.dao.ResourceGenericCMElementDAO;
import se.cambio.cm.model.template.dto.TemplateDTO;

public class ResourceTemplateDAO extends ResourceGenericCMElementDAO<TemplateDTO> {

    public ResourceTemplateDAO() {
        super(TemplateDTO.class);
    }
}
