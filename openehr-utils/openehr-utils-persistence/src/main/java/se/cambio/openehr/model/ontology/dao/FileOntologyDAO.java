package se.cambio.openehr.model.ontology.dao;

import se.cambio.openehr.model.cm.element.dao.FileGenericCMElementDAO;
import se.cambio.openehr.model.ontology.dto.OntologyDTO;
import se.cambio.openehr.util.UserConfigurationManager;

import java.util.Collections;

public class FileOntologyDAO extends FileGenericCMElementDAO<OntologyDTO> {

    public FileOntologyDAO() {
        super(OntologyDTO.class, UserConfigurationManager.getOntologiesFolder(), Collections.singleton("owl"));
    }
}
