package se.cambio.cm.model.archetype.dao;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.generic.dao.ResourceGenericCMElementDAO;

@Component("ArchetypeDAO")
@Profile("cm-admin-resource-dao")
public class ResourceArchetypeDAO extends ResourceGenericCMElementDAO<ArchetypeDTO> {

    public ResourceArchetypeDAO() {
        super(ArchetypeDTO.class);
    }
}
