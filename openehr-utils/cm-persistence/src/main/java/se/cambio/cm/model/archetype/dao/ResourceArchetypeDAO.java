package se.cambio.cm.model.archetype.dao;

import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.cm.element.dao.ResourceGenericCMElementDAO;

public class ResourceArchetypeDAO extends ResourceGenericCMElementDAO<ArchetypeDTO> {

    public ResourceArchetypeDAO() {
        super(ArchetypeDTO.class);
    }
}
