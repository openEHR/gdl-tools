package se.cambio.cm.model.guide.dao;

import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.cm.model.cm.element.dao.ResourceGenericCMElementDAO;

import java.util.Collections;

public class ResourceGuideDAO extends ResourceGenericCMElementDAO<GuideDTO> {

    public ResourceGuideDAO() {
        super(GuideDTO.class, Collections.singleton("gdl"));
    }
}
