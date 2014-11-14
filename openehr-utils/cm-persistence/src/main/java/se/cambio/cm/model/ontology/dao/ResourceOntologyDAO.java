package se.cambio.cm.model.ontology.dao;

import se.cambio.cm.model.cm.element.dao.ResourceGenericCMElementDAO;
import se.cambio.cm.model.ontology.dto.OntologyDTO;

public class ResourceOntologyDAO extends ResourceGenericCMElementDAO<OntologyDTO> {

    public ResourceOntologyDAO() {
        super(OntologyDTO.class);
    }
}
