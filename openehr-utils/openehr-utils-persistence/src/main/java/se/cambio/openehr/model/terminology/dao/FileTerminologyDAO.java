package se.cambio.openehr.model.terminology.dao;

import se.cambio.openehr.model.cm.element.dao.FileGenericCMElementDAO;
import se.cambio.openehr.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.util.UserConfigurationManager;

import java.util.Collections;

public class FileTerminologyDAO extends FileGenericCMElementDAO<TerminologyDTO> {

    public FileTerminologyDAO() {
        super(TerminologyDTO.class, UserConfigurationManager.getTerminologiesFolder(), Collections.singleton("csv"));
    }
}
