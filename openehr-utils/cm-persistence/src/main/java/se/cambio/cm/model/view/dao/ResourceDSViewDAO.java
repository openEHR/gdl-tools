package se.cambio.cm.model.view.dao;

import se.cambio.cm.model.cm.element.dao.ResourceGenericCMElementDAO;
import se.cambio.cm.model.view.dto.DSViewDTO;

public class ResourceDSViewDAO extends ResourceGenericCMElementDAO<DSViewDTO> {

    public ResourceDSViewDAO() {
        super(DSViewDTO.class);
    }
}
