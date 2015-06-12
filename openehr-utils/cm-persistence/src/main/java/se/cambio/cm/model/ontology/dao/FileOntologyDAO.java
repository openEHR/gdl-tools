package se.cambio.cm.model.ontology.dao;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import se.cambio.cm.model.generic.dao.FileGenericCMElementDAO;
import se.cambio.cm.model.ontology.dto.OntologyDTO;
import se.cambio.openehr.util.UserConfigurationManager;

@Component
@Profile("cm-admin-file-dao")
public class FileOntologyDAO extends FileGenericCMElementDAO<OntologyDTO> {

    public FileOntologyDAO() {
        super(OntologyDTO.class, UserConfigurationManager.getOntologiesFolder());
    }
}
