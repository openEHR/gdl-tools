package se.cambio.cm.model.guide.dao;

import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.cm.model.cm.element.dao.FileGenericCMElementDAO;
import se.cambio.openehr.util.UserConfigurationManager;

import java.util.Collections;

public class FileGuideDAO extends FileGenericCMElementDAO<GuideDTO> {

    public FileGuideDAO() {
        super(GuideDTO.class, UserConfigurationManager.getGuidesFolder(), Collections.singleton("gdl"));
    }
}
