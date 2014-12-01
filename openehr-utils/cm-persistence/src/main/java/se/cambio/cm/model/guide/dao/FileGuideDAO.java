package se.cambio.cm.model.guide.dao;

import se.cambio.cm.model.generic.dao.FileGenericCMElementDAO;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.openehr.util.UserConfigurationManager;

public class FileGuideDAO extends FileGenericCMElementDAO<GuideDTO> {

    public FileGuideDAO() {
        super(GuideDTO.class, UserConfigurationManager.getGuidesFolder());
    }
}
