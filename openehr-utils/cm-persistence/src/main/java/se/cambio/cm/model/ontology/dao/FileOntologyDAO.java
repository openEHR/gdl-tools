package se.cambio.cm.model.ontology.dao;

import se.cambio.cm.model.generic.dao.FileGenericCMElementDAO;
import se.cambio.cm.model.ontology.dto.OntologyDTO;
import se.cambio.openehr.util.UserConfigurationManager;

public class FileOntologyDAO extends FileGenericCMElementDAO<OntologyDTO> {

    public FileOntologyDAO() {
        super(OntologyDTO.class, UserConfigurationManager.getOntologiesFolder());
    }
}
