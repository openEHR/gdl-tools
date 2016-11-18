package se.cambio.cm.model.archetype.dao;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.generic.dao.FileGenericCMElementDAO;
import se.cambio.openehr.util.UserConfigurationManager;

@Component("ArchetypeDAO")
@Profile("cm-admin-file-dao")
public class FileArchetypeDAO extends FileGenericCMElementDAO<ArchetypeDTO> {

    public FileArchetypeDAO() {
        super(ArchetypeDTO.class, UserConfigurationManager.instance().getArchetypeFolder());
    }
}
