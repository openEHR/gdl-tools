package se.cambio.openehr.model.terminology.dao;

import se.cambio.openehr.model.cm.element.dao.ResourceGenericCMElementDAO;
import se.cambio.openehr.model.terminology.dto.TerminologyDTO;

import java.util.Collections;

public class ResourceTerminologyDAO extends ResourceGenericCMElementDAO<TerminologyDTO> {

    public ResourceTerminologyDAO() {
        super(TerminologyDTO.class, Collections.singleton("csv"));
    }
}
