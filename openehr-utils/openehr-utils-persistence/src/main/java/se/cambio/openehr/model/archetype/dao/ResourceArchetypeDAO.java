package se.cambio.openehr.model.archetype.dao;

import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.cm.element.dao.ResourceGenericCMElementDAO;

import java.util.Collections;

public class ResourceArchetypeDAO extends ResourceGenericCMElementDAO<ArchetypeDTO> {

    public ResourceArchetypeDAO() {
        super(ArchetypeDTO.class, Collections.singleton("adl"));
    }
}
