package se.cambio.openehr.model.template.dao;

import se.cambio.openehr.model.cm.element.dao.FileGenericCMElementDAO;
import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.util.UserConfigurationManager;

import java.util.Collections;

public class FileTemplateDAO extends FileGenericCMElementDAO<TemplateDTO> {

    public FileTemplateDAO() {
        super(TemplateDTO.class, UserConfigurationManager.getTemplateFolder(), Collections.singleton("oet"));
    }
}
