package se.cambio.cm.model.guide.dao;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import se.cambio.cm.model.generic.dao.ResourceGenericCMElementDAO;
import se.cambio.cm.model.guide.dto.GuideDTO;

@Component("GuideDAO")
@Profile("cm-admin-resource-dao")
public class ResourceGuideDAO extends ResourceGenericCMElementDAO<GuideDTO> {

    public ResourceGuideDAO() {
        super(GuideDTO.class);
    }
}
