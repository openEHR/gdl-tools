package se.cambio.cm.model.terminology.dao;

import se.cambio.cm.model.generic.dao.ResourceGenericCMElementDAO;
import se.cambio.cm.model.terminology.dto.TerminologyDTO;

public class ResourceTerminologyDAO extends ResourceGenericCMElementDAO<TerminologyDTO> {

    public ResourceTerminologyDAO() {
        super(TerminologyDTO.class);
    }
}
