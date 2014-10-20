package se.cambio.cds.model.guideline.dao;

import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.openehr.model.cm.element.dao.ResourceGenericCMElementDAO;

import java.util.Collections;

public class ResourceGuideDAO extends ResourceGenericCMElementDAO<GuideDTO> {

    public ResourceGuideDAO() {
        super(GuideDTO.class, Collections.singleton("gdl"));
    }
}
