package se.cambio.cm.model.template.dao;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import se.cambio.cm.model.generic.dao.FileGenericCMElementDAO;
import se.cambio.cm.model.template.dto.TemplateDTO;
import se.cambio.openehr.util.UserConfigurationManager;

@Component("TemplateDAO")
@Profile("cm-admin-file-dao")
public class FileTemplateDAO extends FileGenericCMElementDAO<TemplateDTO> {

    public FileTemplateDAO() {
        super(TemplateDTO.class, UserConfigurationManager.instance().getTemplateFolder());
    }
}
