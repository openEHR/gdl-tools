package se.cambio.cm.model.terminology.dao;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import se.cambio.cm.model.generic.dao.ResourceGenericCMElementDAO;
import se.cambio.cm.model.terminology.dto.TerminologyDTO;

@Component("TerminologyDAO")
@Profile("cm-admin-resource-dao")
public class ResourceTerminologyDAO extends ResourceGenericCMElementDAO<TerminologyDTO> {

    public ResourceTerminologyDAO() {
        super(TerminologyDTO.class);
    }
}
