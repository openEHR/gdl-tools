package se.cambio.cm.model.ontology.dao;

import se.cambio.cm.model.cm.element.dao.FileGenericCMElementDAO;
import se.cambio.cm.model.ontology.dto.OntologyDTO;
import se.cambio.openehr.util.UserConfigurationManager;

import java.util.Collections;

public class FileOntologyDAO extends FileGenericCMElementDAO<OntologyDTO> {

    public FileOntologyDAO() {
        super(OntologyDTO.class, UserConfigurationManager.getOntologiesFolder(), Collections.singleton("owl"));
    }
}
