package se.cambio.cm.model.template.dao;

import se.cambio.cm.model.generic.dao.FileGenericCMElementDAO;
import se.cambio.cm.model.template.dto.TemplateDTO;
import se.cambio.openehr.util.UserConfigurationManager;

public class FileTemplateDAO extends FileGenericCMElementDAO<TemplateDTO> {

    public FileTemplateDAO() {
        super(TemplateDTO.class, UserConfigurationManager.getTemplateFolder());
    }
}
