package se.cambio.cm.model.template.dao;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import se.cambio.cm.model.generic.dao.ResourceGenericCMElementDAO;
import se.cambio.cm.model.template.dto.TemplateDTO;

@Component("TemplateDAO")
@Profile("cm-admin-resource-dao")
public class ResourceTemplateDAO extends ResourceGenericCMElementDAO<TemplateDTO> {

    public ResourceTemplateDAO() {
        super(TemplateDTO.class);
    }
}
