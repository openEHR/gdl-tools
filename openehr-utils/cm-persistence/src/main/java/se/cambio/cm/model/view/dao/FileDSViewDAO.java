package se.cambio.cm.model.view.dao;

import se.cambio.cm.model.view.dto.DSViewDTO;
import se.cambio.cm.model.cm.element.dao.FileGenericCMElementDAO;
import se.cambio.openehr.util.UserConfigurationManager;

import java.util.Collections;

public class FileDSViewDAO extends FileGenericCMElementDAO<DSViewDTO> {

    public FileDSViewDAO() {
        super(DSViewDTO.class, UserConfigurationManager.getDSViewsFolder(), Collections.singleton("dsv"));
    }
}
