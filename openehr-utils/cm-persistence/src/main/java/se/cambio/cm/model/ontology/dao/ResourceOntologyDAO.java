package se.cambio.cm.model.ontology.dao;

import se.cambio.cm.model.generic.dao.ResourceGenericCMElementDAO;
import se.cambio.cm.model.ontology.dto.OntologyDTO;

public class ResourceOntologyDAO extends ResourceGenericCMElementDAO<OntologyDTO> {

    public ResourceOntologyDAO() {
        super(OntologyDTO.class);
    }
}
