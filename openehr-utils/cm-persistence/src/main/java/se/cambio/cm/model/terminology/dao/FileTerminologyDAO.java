package se.cambio.cm.model.terminology.dao;

import se.cambio.cm.model.cm.element.dao.FileGenericCMElementDAO;
import se.cambio.cm.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.util.UserConfigurationManager;

import java.util.Collections;

public class FileTerminologyDAO extends FileGenericCMElementDAO<TerminologyDTO> {

    public FileTerminologyDAO() {
        super(TerminologyDTO.class, UserConfigurationManager.getTerminologiesFolder(), Collections.singleton("csv"));
    }
}
