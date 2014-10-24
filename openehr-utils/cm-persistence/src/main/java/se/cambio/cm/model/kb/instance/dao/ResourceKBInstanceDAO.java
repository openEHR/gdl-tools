package se.cambio.cm.model.kb.instance.dao;

import se.cambio.cm.model.cm.element.dao.ResourceGenericCMElementDAO;
import se.cambio.cm.model.kb.instance.dto.KBInstanceDTO;

import java.util.Collections;

public class ResourceKBInstanceDAO extends ResourceGenericCMElementDAO<KBInstanceDTO> {

    public ResourceKBInstanceDAO() {
        super(KBInstanceDTO.class, Collections.singleton("kbi"));
    }
}
