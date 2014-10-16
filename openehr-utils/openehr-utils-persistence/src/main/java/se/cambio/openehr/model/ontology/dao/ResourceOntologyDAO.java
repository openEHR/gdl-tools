package se.cambio.openehr.model.ontology.dao;

import se.cambio.openehr.model.cm.element.dao.ResourceGenericCMElementDAO;
import se.cambio.openehr.model.ontology.dto.OntologyDTO;

import java.util.Collections;

public class ResourceOntologyDAO extends ResourceGenericCMElementDAO<OntologyDTO> {

    public ResourceOntologyDAO() {
        super(OntologyDTO.class, Collections.singleton("owl"));
    }
}
