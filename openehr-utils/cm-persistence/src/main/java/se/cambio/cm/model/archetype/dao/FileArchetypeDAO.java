package se.cambio.cm.model.archetype.dao;

import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.cm.element.dao.FileGenericCMElementDAO;
import se.cambio.openehr.util.UserConfigurationManager;

public class FileArchetypeDAO extends FileGenericCMElementDAO<ArchetypeDTO> {

    public FileArchetypeDAO() {
        super(ArchetypeDTO.class, UserConfigurationManager.getArchetypeFolder());
    }
}
