package se.cambio.cm.model.terminology.dao;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import se.cambio.cm.model.generic.dao.FileGenericCMElementDAO;
import se.cambio.cm.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.util.UserConfigurationManager;

@Component("TerminologyDAO")
@Profile("cm-admin-file-dao")
public class FileTerminologyDAO extends FileGenericCMElementDAO<TerminologyDTO> {

    public FileTerminologyDAO() {
        super(TerminologyDTO.class, UserConfigurationManager.getTerminologiesFolder());
    }
}
