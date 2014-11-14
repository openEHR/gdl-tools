package se.cambio.cm.model.view.dao;

import se.cambio.cm.model.cm.element.dao.FileGenericCMElementDAO;
import se.cambio.cm.model.view.dto.DSViewDTO;
import se.cambio.openehr.util.UserConfigurationManager;

public class FileDSViewDAO extends FileGenericCMElementDAO<DSViewDTO> {

    public FileDSViewDAO() {
        super(DSViewDTO.class, UserConfigurationManager.getDSViewsFolder());
    }
}
