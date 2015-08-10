package se.cambio.cm.model.ontology.dao;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import se.cambio.cm.model.generic.dao.ResourceGenericCMElementDAO;
import se.cambio.cm.model.ontology.dto.OntologyDTO;

@Component("OntologyDAO")
@Profile("cm-admin-resource-dao")
public class ResourceOntologyDAO extends ResourceGenericCMElementDAO<OntologyDTO> {

    public ResourceOntologyDAO() {
        super(OntologyDTO.class);
    }
}
