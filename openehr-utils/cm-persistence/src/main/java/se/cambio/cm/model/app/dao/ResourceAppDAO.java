package se.cambio.cm.model.app.dao;

import se.cambio.cm.model.app.dto.CDSAppDTO;
import se.cambio.cm.model.cm.element.dao.ResourceGenericCMElementDAO;

import java.util.Collections;

public class ResourceAppDAO extends ResourceGenericCMElementDAO<CDSAppDTO> {

    public ResourceAppDAO() {
        super(CDSAppDTO.class, Collections.singleton("app"));
    }
}
