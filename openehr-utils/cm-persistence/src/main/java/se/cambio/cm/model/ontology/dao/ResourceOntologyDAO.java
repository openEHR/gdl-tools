package se.cambio.cm.model.ontology.dao;

import se.cambio.cm.model.cm.element.dao.ResourceGenericCMElementDAO;
import se.cambio.cm.model.ontology.dto.OntologyDTO;

import java.util.Collections;

public class ResourceOntologyDAO extends ResourceGenericCMElementDAO<OntologyDTO> {

    public ResourceOntologyDAO() {
        super(OntologyDTO.class, Collections.singleton("owl"));
    }
}
