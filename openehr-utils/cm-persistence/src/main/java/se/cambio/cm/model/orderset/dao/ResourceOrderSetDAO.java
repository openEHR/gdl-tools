package se.cambio.cm.model.orderset.dao;

import se.cambio.cm.model.cm.element.dao.ResourceGenericCMElementDAO;
import se.cambio.cm.model.orderset.dto.OrderSetDTO;

import java.util.Collections;

public class ResourceOrderSetDAO extends ResourceGenericCMElementDAO<OrderSetDTO> {

    public ResourceOrderSetDAO() {
        super(OrderSetDTO.class, Collections.singleton("ost"));
    }
}
