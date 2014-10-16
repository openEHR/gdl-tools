package se.cambio.openehr.model.archetype.dao;

import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.cm.element.dao.FileGenericCMElementDAO;
import se.cambio.openehr.util.UserConfigurationManager;

import java.util.Collections;

public class FileArchetypeDAO extends FileGenericCMElementDAO<ArchetypeDTO> {

    public FileArchetypeDAO() {
        super(ArchetypeDTO.class, UserConfigurationManager.getArchetypeFolder(), Collections.singleton("adl"));
    }
}
